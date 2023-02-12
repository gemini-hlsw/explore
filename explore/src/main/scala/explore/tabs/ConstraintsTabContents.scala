// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import clue.TransactionalClient
import crystal.Pot
import crystal.implicits.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.numeric.NonNegInt
import explore.Icons
import explore.*
import explore.common.ConstraintGroupQueries.*
import explore.common.TimingQueries.*
import explore.common.UserPreferencesQueries.*
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.constraints.ConstraintsPanel
import explore.constraints.ConstraintsSummaryTable
import explore.constraints.TimingWindowsPanel
import explore.model.*
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.layout.*
import explore.model.layout.unsafe.given
import explore.model.reusability.given
import explore.model.reusability.given
import explore.observationtree.ConstraintGroupObsList
import explore.optics.*
import explore.optics.all.*
import explore.shortcuts.*
import explore.shortcuts.given
import explore.syntax.ui.*
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.callback.CallbackCatsEffect.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.refined.*
import lucuma.refined.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import monocle.Focus
import org.scalajs.dom.window
import org.typelevel.log4cats.Logger
import queries.common.ConstraintGroupQueriesGQL.*
import queries.common.ObsQueriesGQL
import queries.common.TimingWindowsGQL.*
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.UserPreferencesDB
import react.common.ReactFnProps
import react.draggable.Axis
import react.fa.*
import react.gridlayout.*
import react.hotkeys.*
import react.hotkeys.hooks.*
import react.resizeDetector.*
import react.resizeDetector.hooks.*

import scala.collection.immutable.SortedSet
import scala.concurrent.duration.*

case class ConstraintsTabContents(
  userId:         Option[User.Id],
  programId:      Program.Id,
  focusedObsSet:  Option[ObsIdSet],
  expandedIds:    View[SortedSet[ObsIdSet]],
  listUndoStacks: View[UndoStacks[IO, ConstraintGroupList]],
  // TODO: Clean up the groupUndoStack somewhere, somehow?
  groupUndoStack: View[Map[ObsIdSet, UndoStacks[IO, ConstraintSet]]]
) extends ReactFnProps(ConstraintsTabContents.component)

object ConstraintsTabContents extends TwoPanels:
  private type Props = ConstraintsTabContents

  private val ConstraintsHeight: NonNegInt        = 4.refined
  private val ConstraintsSmallHeight: NonNegInt   = 7.refined
  private val TimingWindowsHeight: NonNegInt      = 14.refined
  private val TimingWindowsSmallHeight: NonNegInt = 12.refined
  private val TileMinWidth: NonNegInt             = 6.refined
  private val DefaultWidth: NonNegInt             = 10.refined
  private val DefaultLargeWidth: NonNegInt        = 12.refined

  private val layoutMedium: Layout = Layout(
    List(
      LayoutItem(
        i = ObsTabTilesIds.ConstraintsId.id.value,
        x = 0,
        y = 0,
        w = DefaultWidth.value,
        h = ConstraintsHeight.value,
        isResizable = false
      ),
      LayoutItem(
        i = ObsTabTilesIds.TimingWindowsId.id.value,
        x = 0,
        y = ConstraintsHeight.value,
        w = DefaultWidth.value,
        h = TimingWindowsHeight.value,
        isResizable = false
      )
    )
  )

  private val defaultConstraintsLayouts = defineStdLayouts(
    Map(
      (BreakpointName.lg,
       layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
      ),
      (BreakpointName.md, layoutMedium)
    )
  )

  private def renderFn(
    props:                 Props,
    state:                 View[SelectedPanel],
    defaultLayouts:        LayoutsMap,
    layouts:               View[Pot[LayoutsMap]],
    resize:                UseResizeDetectorReturn,
    ctx:                   AppContext[IO]
  )(
    constraintsAndWindows: View[(TimingWindowResult, ConstraintSummaryWithObervations)]
  ): VdomNode = {

    val constraintsWithObs = constraintsAndWindows.zoom(
      Focus[(TimingWindowResult, ConstraintSummaryWithObervations)](_._2)
    )

    val timingWindows = constraintsAndWindows.zoom(
      Focus[(TimingWindowResult, ConstraintSummaryWithObervations)](_._1)
    )

    def constraintsTree(constraintWithObs: View[ConstraintSummaryWithObervations]) =
      ConstraintGroupObsList(
        constraintWithObs,
        props.programId,
        props.focusedObsSet,
        state.set(SelectedPanel.Summary),
        props.expandedIds,
        props.listUndoStacks
      )

    def findConstraintGroup(
      obsIds: ObsIdSet,
      cgl:    ConstraintGroupList
    ): Option[ConstraintGroup] =
      cgl.find(_._1.intersect(obsIds).nonEmpty).map(_._2)

    def onModSummaryWithObs(
      groupObsIds:  ObsIdSet,
      editedObsIds: ObsIdSet
    )(cswo:         ConstraintSummaryWithObervations): Callback = {
      val groupList = cswo.constraintGroups

      val updateExpanded = findConstraintGroup(editedObsIds, groupList).fold(Callback.empty) { cg =>
        // We should always find the constraint group.
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
      makeBackButton(props.programId, AppTab.Constraints, state, ctx)

    val rightSide = (_: UseResizeDetectorReturn) =>
      props.focusedObsSet
        .flatMap(ids =>
          findConstraintGroup(ids, constraintsWithObs.get.constraintGroups).map(cg => (ids, cg))
        )
        .fold[VdomNode] {
          Tile(
            "constraints".refined,
            "Constraints Summary",
            backButton.some
          )(renderInTitle =>
            ConstraintsSummaryTable(
              props.userId,
              props.programId,
              constraintsWithObs.get.constraintGroups,
              props.expandedIds,
              renderInTitle
            )
          )
        } { case (idsToEdit, constraintGroup) =>
          val groupObsIds   = constraintGroup.obsIds
          val constraintSet = constraintGroup.constraintSet
          val cglView       = constraintsWithObs
            .withOnMod(onModSummaryWithObs(groupObsIds, idsToEdit))
            .zoom(ConstraintSummaryWithObervations.constraintGroups)

          val getCs: ConstraintGroupList => ConstraintSet = _ => constraintSet

          def modCs(
            mod: ConstraintSet => ConstraintSet
          ): ConstraintGroupList => ConstraintGroupList =
            cgl =>
              findConstraintGroup(idsToEdit, cgl)
                .map { cg =>
                  val newCg        = ConstraintGroup.constraintSet.modify(mod)(cg)
                  // see if the edit caused a merger
                  val mergeWithIds = cgl
                    .find { case (ids, group) =>
                      !ids.intersects(idsToEdit) && group.constraintSet === newCg.constraintSet
                    }
                    .map(_._1)

                  // If we're editing an observation within a larger group, we need a split
                  val splitList =
                    if (idsToEdit === groupObsIds)
                      cgl.updated(groupObsIds, newCg) // otherwise, just update current group
                    else {
                      val diffIds = groupObsIds.removeUnsafe(idsToEdit)
                      cgl
                        .removed(groupObsIds)
                        .updated(idsToEdit, ConstraintGroup(newCg.constraintSet, idsToEdit))
                        .updated(diffIds, ConstraintGroup(cg.constraintSet, diffIds))
                    }

                  mergeWithIds.fold(splitList) { idsToMerge =>
                    val combined = idsToMerge ++ idsToEdit
                    splitList
                      .removed(idsToMerge)
                      .removed(idsToEdit)
                      .updated(combined, ConstraintGroup(newCg.constraintSet, combined))
                  }
                }
                .getOrElse(cgl) // shouldn't happen

          val csView: View[ConstraintSet] =
            cglView
              .zoom(getCs)(modCs)

          val csUndo: View[UndoStacks[IO, ConstraintSet]] =
            props.groupUndoStack.zoom(atMapWithDefault(idsToEdit, UndoStacks.empty))

          val constraintsTitle = idsToEdit.single match
            case Some(id) => s"Observation $id"
            case None     => s"Editing Constraints for ${idsToEdit.size} Observations"

          val constraintsTile = Tile(
            ObsTabTilesIds.ConstraintsId.id,
            constraintsTitle,
            backButton.some,
            canMinimize = true
          )(renderInTitle =>
            ConstraintsPanel(props.programId, idsToEdit.toList, csView, csUndo, renderInTitle)
          )

          val timingWindowsView = timingWindows.zoom(TimingWindowsList)
          val timingWindowsTile =
            Tile(ObsTabTilesIds.TimingWindowsId.id, "Timing Windows", canMinimize = true)(
              renderInTitle =>
                TimingWindowsPanel(timingWindowsView)
                  .withKey(s"timing-window-${timingWindowsView.get.length}")
            )

          val rglRender: LayoutsMap => VdomNode = (l: LayoutsMap) =>
            TileController(
              props.userId,
              resize.width.getOrElse(1),
              defaultLayouts,
              l,
              List(constraintsTile, timingWindowsTile),
              GridLayoutSection.ConstraintsLayout,
              None
            )
          potRender[LayoutsMap](rglRender)(layouts.get)
        }

    makeOneOrTwoPanels(
      state,
      constraintsTree(constraintsWithObs),
      rightSide,
      RightSideCardinality.Multi,
      resize
    )
  }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useGlobalHotkeysWithDepsBy((props, ctx) => props.programId) { (props, ctx) => pid =>
        import ctx.given

        def callbacks: ShortcutCallbacks = { case GoToSummary =>
          ctx.setPageVia(AppTab.Constraints, pid, Focused.None, SetRouteVia.HistoryPush)
        }
        UseHotkeysProps(List(GoToSummary).toHotKeys, callbacks)
      }
      // Initial target layout
      .useStateView(Pot.pending[LayoutsMap])
      // Keep a record of the initial target layout
      .useMemo(())(_ => defaultConstraintsLayouts)
      // Load the config from user prefrences
      .useEffectWithDepsBy((p, _, _, _) => p.userId) { (props, ctx, layout, defaultLayout) => _ =>
        import ctx.given

        GridLayouts
          .queryWithDefault[IO](
            props.userId,
            GridLayoutSection.ConstraintsLayout,
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
                .to[IO]
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
      .useStreamResourceViewOnMountBy { (props, ctx, _, _, _) =>
        import ctx.given

        (TimingWindowsQuery.query(), ConstraintGroupObsQuery.query(props.programId))
          .mapN((tw, cg) => (tw, ConstraintGroupObsQuery.Data.asConstraintSummWithObs.get(cg)))
          .reRunOnResourceSignals(
            ObsQueriesGQL.ProgramObservationsEditSubscription.subscribe[IO](props.programId),
            TimingWindowSubscription.subscribe[IO]()
          )
      }
      // Measure its size
      .useResizeDetector()
      .render { (props, ctx, layout, defaultLayout, state, constraintsWithObs, resize) =>
        React.Fragment(
          constraintsWithObs.render(
            renderFn(props, state, defaultLayout, layout, resize, ctx) _,
            <.span(DefaultPendingRender).withRef(resize.ref)
          )
        )

      }
