// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import explore.*
import explore.common.TimingWindowsQueries
import explore.components.FocusedStatus
import explore.components.Tile
import explore.components.TileController
import explore.data.KeyedIndexedList
import explore.model.*
import explore.model.AppContext
import explore.model.ObservationList
import explore.model.ProgramSummaries
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
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
import lucuma.react.common.ReactFnProps
import lucuma.react.hotkeys.*
import lucuma.react.hotkeys.hooks.*
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import monocle.Iso

import scala.collection.immutable.SortedSet
import scala.scalajs.LinkingInfo

case class SchedulingTabContents(
  userId:           Option[User.Id],
  programId:        Program.Id,
  programSummaries: UndoContext[ProgramSummaries],
  userPreferences:  UserPreferences,
  focusedObsSet:    Option[ObsIdSet],
  expandedIds:      View[SortedSet[ObsIdSet]],
  readonly:         Boolean
) extends ReactFnProps(SchedulingTabContents.component)

object SchedulingTabContents extends TwoPanels:
  private type Props = SchedulingTabContents

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
      .useStateView[SelectedPanel](SelectedPanel.Uninitialized)
      .useEffectWithDepsBy((props, _, state) => (props.focusedObsSet, state.reuseByValue)) {
        (_, _, _) => params =>
          val (focusedObsSet, selected) = params
          (focusedObsSet, selected.get) match {
            case (Some(_), _)                 => selected.set(SelectedPanel.Editor)
            case (None, SelectedPanel.Editor) => selected.set(SelectedPanel.Summary)
            case _                            => Callback.empty
          }
      }
      // Measure its size
      .useResizeDetector()
      .render { (props, ctx, state, resize) =>
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

              val timingWindows: View[List[TimingWindow]] =
                TimingWindowsQueries.viewWithRemoteMod(
                  idsToEdit,
                  observations
                    .undoableView[List[TimingWindow]](
                      twTraversal.getAll.andThen(_.head),
                      twTraversal.modify
                    )
                )

              val timingWindowsTile =
                TimingWindowsPanel.timingWindowsPanel(timingWindows, props.readonly)

              TileController(
                props.userId,
                resize.width.getOrElse(1),
                ExploreGridLayouts.sectionLayout(GridLayoutSection.SchedulingLayout),
                props.userPreferences.schedulingTabLayout,
                List(timingWindowsTile),
                GridLayoutSection.SchedulingLayout,
                None
              )
            }

        val schedulingTree =
          SchedulingGroupObsList(
            props.programId,
            observations,
            props.programSummaries,
            props.programSummaries.get.schedulingGroups,
            props.programSummaries.get.obsExecutionPots,
            props.focusedObsSet,
            state.set(SelectedPanel.Summary),
            props.expandedIds,
            props.readonly
          )

        React.Fragment(
          if (LinkingInfo.developmentMode)
            FocusedStatus(AppTab.Scheduling, props.programId, Focused(props.focusedObsSet))
          else EmptyVdom,
          makeOneOrTwoPanels(state, schedulingTree, rightSide, RightSideCardinality.Multi, resize)
        )
      }
