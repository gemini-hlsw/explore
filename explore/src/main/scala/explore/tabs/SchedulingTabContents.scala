// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import _root_.react.common.ReactFnProps
import _root_.react.gridlayout.*
import _root_.react.hotkeys.*
import _root_.react.hotkeys.hooks.*
import _root_.react.resizeDetector.*
import _root_.react.resizeDetector.hooks.*
import cats.effect.IO
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.numeric.NonNegInt
import explore.*
import explore.common.TimingWindowsQueries
import explore.common.UserPreferencesQueries.*
import explore.components.Tile
import explore.components.TileController
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
import explore.observationtree.SchedulingGroupObsList
import explore.shortcuts.*
import explore.shortcuts.given
import explore.timingwindows.TimingWindowsPanel
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.callback.CallbackCatsEffect.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.TimingWindow
import lucuma.core.model.User
import lucuma.refined.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Iso

import scala.collection.immutable.SortedSet

case class SchedulingTabContents(
  userId:           Option[User.Id],
  programId:        Program.Id,
  programSummaries: UndoContext[ProgramSummaries],
  focusedObsSet:    Option[ObsIdSet],
  expandedIds:      View[SortedSet[ObsIdSet]]
) extends ReactFnProps(SchedulingTabContents.component)

object SchedulingTabContents extends TwoPanels:
  private type Props = SchedulingTabContents

  private val SchedulingHeight: NonNegInt  = 14.refined
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

        def findSchedulingGroup(
          obsIds: ObsIdSet,
          cgl:    SchedulingGroupList
        ): Option[SchedulingGroup] =
          cgl.find(_._1.intersect(obsIds).nonEmpty).map(SchedulingGroup.fromTuple)

        val observations: UndoSetter[ObservationList] =
          props.programSummaries.zoom(ProgramSummaries.observations)

        val rightSide = (_: UseResizeDetectorReturn) =>
          props.focusedObsSet
            .flatMap(ids =>
              findSchedulingGroup(ids, props.programSummaries.get.schedulingGroups)
                .map(cg => (ids, cg))
            )
            .fold[VdomNode] {
              <.div("Nothing selected - Will we have a summary table?")
            } { case (idsToEdit, schedulingGroup) =>
              val obsTraversal = Iso
                .id[ObservationList]
                .filterIndex((id: Observation.Id) => idsToEdit.contains(id))
                .andThen(KeyedIndexedList.value)

              val twTraversal = obsTraversal.andThen(ObsSummary.timingWindows)

              val twView: View[List[TimingWindow]] =
                TimingWindowsQueries.viewWithRemoteMod(
                  props.programId,
                  idsToEdit,
                  observations
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
            observations,
            props.programSummaries,
            props.programSummaries.get.schedulingGroups,
            props.focusedObsSet,
            state.set(SelectedPanel.Summary),
            props.expandedIds
          )

        makeOneOrTwoPanels(state, schedulingTree, rightSide, RightSideCardinality.Multi, resize)
      }
