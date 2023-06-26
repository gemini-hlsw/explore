// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import clue.FetchClient
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.numeric.NonNegInt
import explore.*
import explore.cache.ProgramCache
import explore.common.TimingWindowsQueries
import explore.common.UserPreferencesQueries.*
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.data.KeyedIndexedList
import explore.model.ObservationList
import explore.model.ProgramSummaries
import explore.model.*
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.layout.*
import explore.model.layout.unsafe.given
import explore.model.reusability.given
import explore.model.reusability.given
import explore.observationtree.SchedulingGroupObsList
import explore.optics.*
import explore.optics.all.*
import explore.shortcuts.*
import explore.shortcuts.given
import explore.syntax.ui.*
import explore.timingwindows.TimingWindowsPanel
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.callback.CallbackCatsEffect.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.TimingWindow
import lucuma.core.model.User
import lucuma.refined.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.DefaultPendingRender
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import monocle.Focus
import monocle.Iso
import org.scalajs.dom.window
import org.typelevel.log4cats.Logger
import queries.common.ObsQueriesGQL
import queries.common.ObsQueriesGQL.UpdateObservationMutation
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.UserPreferencesDB
import _root_.react.common.ReactFnProps
import _root_.react.draggable.Axis
import _root_.react.fa.*
import _root_.react.gridlayout.*
import _root_.react.hotkeys.*
import _root_.react.hotkeys.hooks.*
import _root_.react.resizeDetector.*
import _root_.react.resizeDetector.hooks.*

import scala.collection.immutable.SortedSet
import scala.concurrent.duration.*

case class SchedulingTabContents(
  userId:           Option[User.Id],
  programId:        Program.Id,
  programSummaries: View[ProgramSummaries],
  focusedObsSet:    Option[ObsIdSet],
  expandedIds:      View[SortedSet[ObsIdSet]],
  obsUndoStacks:    View[UndoStacks[IO, ObservationList]]
) extends ReactFnProps(SchedulingTabContents.component)

object SchedulingTabContents extends TwoPanels:
  private type Props = SchedulingTabContents

  private val SchedulingHeight: NonNegInt  = 14.refined
  private val TileMinWidth: NonNegInt      = 6.refined
  private val DefaultWidth: NonNegInt      = 10.refined
  private val DefaultLargeWidth: NonNegInt = 12.refined

  private val layoutMedium: Layout = Layout(
    List(
      LayoutItem(
        i = ObsTabTilesIds.TimingWindowsId.id.value,
        x = 0,
        y = 0,
        w = DefaultWidth.value,
        h = SchedulingHeight.value,
        isResizable = false
      )
    )
  )

  private val defaultSchedulingLayouts = defineStdLayouts(
    Map(
      (BreakpointName.lg,
       layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
      ),
      (BreakpointName.md, layoutMedium)
    )
  )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useGlobalHotkeysWithDepsBy((props, ctx) => props.programId) { (props, ctx) => pid =>
        import ctx.given

        def callbacks: ShortcutCallbacks = { case GoToSummary =>
          ctx.setPageVia(AppTab.Scheduling, pid, Focused.None, SetRouteVia.HistoryPush)
        }
        UseHotkeysProps(List(GoToSummary).toHotKeys, callbacks)
      }
      // Initial target layout
      .useStateView(Pot.pending[LayoutsMap])
      // Keep a record of the initial target layout
      .useMemo(())(_ => defaultSchedulingLayouts)
      // Load the config from user prefrences
      .useEffectWithDepsBy((p, _, _, _) => p.userId) { (props, ctx, layout, defaultLayout) => _ =>
        import ctx.given

        GridLayouts
          .queryWithDefault[IO](
            props.userId,
            GridLayoutSection.SchedulingLayout,
            defaultLayout
          )
          .attempt
          .flatMap {
            case Right(dbLayout) =>
              layout
                .mod(
                  _.fold(
                    mergeMap(dbLayout, defaultLayout).ready,
                    _ => mergeMap(dbLayout, defaultLayout).ready,
                    cur => mergeMap(dbLayout, cur).ready
                  )
                )
                .toAsync
            case Left(_)         => IO.unit
          }
      }
      .useStateView[SelectedPanel](SelectedPanel.Uninitialized)
      .useEffectWithDepsBy((props, _, _, _, state) => (props.focusedObsSet, state.reuseByValue)) {
        (_, _, _, _, _) => params =>
          val (focusedObsSet, selected) = params
          (focusedObsSet, selected.get) match {
            case (Some(_), _)                 => selected.set(SelectedPanel.Editor)
            case (None, SelectedPanel.Editor) => selected.set(SelectedPanel.Summary)
            case _                            => Callback.empty
          }
      }
      // Measure its size
      .useResizeDetector()
      .render { (props, ctx, layouts, defaultLayouts, state, resize) =>
        import ctx.given

        val programSummaries: View[ProgramSummaries] = props.programSummaries

        def findSchedulingGroup(
          obsIds: ObsIdSet,
          cgl:    SchedulingGroupList
        ): Option[SchedulingGroup] =
          cgl.find(_._1.intersect(obsIds).nonEmpty).map(SchedulingGroup.fromTuple)

        def onModSummaryWithObs(
          groupObsIds:  ObsIdSet,
          editedObsIds: ObsIdSet
        )(programSummaries: ProgramSummaries): Callback = {
          val groupList: SchedulingGroupList = programSummaries.schedulingGroups

          val updateExpanded =
            findSchedulingGroup(editedObsIds, groupList).fold(Callback.empty) { cg =>
              // We should always find the scheduling group.
              // If a group was edited while closed and it didn't create a merger, keep it closed,
              // otherwise expand all affected groups.
              props.expandedIds
                .mod { eids =>
                  val withOld       =
                    if (groupObsIds === editedObsIds) eids
                    else eids + groupObsIds.removeUnsafe(editedObsIds)
                  val withOldAndNew =
                    if (editedObsIds === cg.obsIds && editedObsIds === groupObsIds) withOld
                    else withOld + cg.obsIds

                  withOldAndNew.filter(ids => groupList.contains(ids)) // clean up
                }
            }

          updateExpanded
        }

        val backButton: VdomNode =
          makeBackButton(props.programId, AppTab.Scheduling, state, ctx)

        val obsView: View[ObservationList] = programSummaries
          // TODO Find another mechanism to update expandeds
          // .withOnMod(onModSummaryWithObs(groupObsIds, idsToEdit))
          .zoom(ProgramSummaries.observations)

        val obsUndoCtx: UndoContext[ObservationList] = UndoContext(props.obsUndoStacks, obsView)

        val rightSide = (_: UseResizeDetectorReturn) =>
          props.focusedObsSet
            .flatMap(ids =>
              findSchedulingGroup(ids, programSummaries.get.schedulingGroups)
                .map(cg => (ids, cg))
            )
            .fold[VdomNode] {
              <.div("Nothing selected - Will we have a summary table?")
            } { case (idsToEdit, schedulingGroup) =>
              val groupObsIds: ObsIdSet = schedulingGroup.obsIds

              val obsTraversal = Iso
                .id[ObservationList]
                .filterIndex((id: Observation.Id) => idsToEdit.contains(id))
                .andThen(KeyedIndexedList.value)

              val twTraversal = obsTraversal.andThen(ObsSummary.timingWindows)

              val twView: View[List[TimingWindow]] =
                TimingWindowsQueries.viewWithRemoteMod(
                  props.programId,
                  idsToEdit,
                  obsUndoCtx
                    .undoableView[List[TimingWindow]](
                      twTraversal.getAll.andThen(_.head),
                      twTraversal.modify
                    )
                )

              val timingWindowsTile =
                Tile(ObsTabTilesIds.TimingWindowsId.id, "Scheduling Windows", canMinimize = true)(
                  renderInTitle => TimingWindowsPanel(twView, renderInTitle)
                )

              layouts.renderPotView(l =>
                TileController(
                  props.userId,
                  resize.width.getOrElse(1),
                  defaultLayouts,
                  l,
                  List(timingWindowsTile),
                  GridLayoutSection.SchedulingLayout,
                  None
                )
              )
            }

        val schedulingTree =
          SchedulingGroupObsList(
            props.programId,
            obsUndoCtx,
            programSummaries.get.schedulingGroups,
            props.focusedObsSet,
            state.set(SelectedPanel.Summary),
            props.expandedIds
          )

        makeOneOrTwoPanels(state, schedulingTree, rightSide, RightSideCardinality.Multi, resize)
      }
