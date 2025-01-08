// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import explore.*
import explore.actions.ObservationPasteIntoSchedulingGroupAction
import explore.common.TimingWindowsQueries
import explore.components.FocusedStatus
import explore.components.TileController
import explore.model.*
import explore.model.AppContext
import explore.model.Observation
import explore.model.ObservationList
import explore.model.ProgramSummaries
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.reusability.given
import explore.observationtree.SchedulingGroupObsList
import explore.schedulingWindows.SchedulingWindowsTile
import explore.shortcuts.*
import explore.shortcuts.given
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.callback.CallbackCatsEffect.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.TimingWindow
import lucuma.core.model.User
import lucuma.react.common.ReactFnProps
import lucuma.react.hotkeys.*
import lucuma.react.hotkeys.hooks.*
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.schemas.ObservationDB
import lucuma.ui.LucumaStyles
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import monocle.Iso
import monocle.Traversal
import org.typelevel.log4cats.Logger
import queries.schemas.odb.ObsQueries

import scala.collection.immutable.SortedSet
import scala.scalajs.LinkingInfo

case class SchedulingTabContents(
  programId:        Program.Id,
  userId:           Option[User.Id],
  programSummaries: UndoContext[ProgramSummaries],
  userPreferences:  UserPreferences,
  focusedObsSet:    Option[ObsIdSet],
  expandedIds:      View[SortedSet[ObsIdSet]],
  readonly:         Boolean
) extends ReactFnProps(SchedulingTabContents.component):
  private val observations: ObservationList =
    programSummaries.zoom(ProgramSummaries.observations).get

object SchedulingTabContents extends TwoPanels:
  private type Props = SchedulingTabContents

  private def applyObs(
    obsIds:           List[(Observation.Id, List[TimingWindow])],
    programSummaries: UndoSetter[ProgramSummaries],
    expandedIds:      View[SortedSet[ObsIdSet]]
  )(using FetchClient[IO, ObservationDB], Logger[IO]): IO[Unit] =
    obsIds
      .traverse: (obsId, timingWindows) =>
        ObsQueries
          .applyObservation[IO](obsId, onTimingWindows = timingWindows.some)
          .map(obs => (obs, timingWindows))
      .flatMap: obsWithConstraintSetList =>
        val newIds: List[(Observation.Id, List[TimingWindow])] =
          obsWithConstraintSetList.map((obs, cs) => (obs.id, cs))

        val observations: List[Observation] =
          obsWithConstraintSetList.map(_._1)

        ObservationPasteIntoSchedulingGroupAction(newIds, expandedIds.async.mod)
          .set(programSummaries)(observations.some)
          .toAsync
      .void

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useState(none[ObsIdSet]) // shadowClipboardObs (a copy as state only if it has observations)
      .useEffectOnMountBy: (_, ctx, shadowClipboardObs) => // initialize shadowClipboard
        import ctx.given
        ExploreClipboard.get.flatMap:
          _ match
            case LocalClipboard.CopiedObservations(idSet) =>
              shadowClipboardObs.setStateAsync(idSet.some)
            case _                                        => IO.unit
      .useCallbackWithDepsBy((props, _, _) => props.focusedObsSet): // COPY Action Callback
        (_, ctx, shadowClipboardObs) =>
          focusedObsSet =>
            import ctx.given

            focusedObsSet
              .map: obsIdSet =>
                (ExploreClipboard
                  .set(LocalClipboard.CopiedObservations(obsIdSet)) >>
                  shadowClipboardObs.setStateAsync(obsIdSet.some))
                  .withToast(s"Copied observation(s) ${obsIdSet.idSet.toList.mkString(", ")}")
              .orUnit
              .runAsync
      .useCallbackWithDepsBy((props, _, _, _) => // PASTE Action Callback
        (props.observations, props.focusedObsSet, props.readonly)
      ): (props, ctx, _, _) =>
        (observations, selObsSet, readonly) =>
          import ctx.given

          ExploreClipboard.get
            .flatMap:
              case LocalClipboard.CopiedObservations(copiedObsIdSet) =>
                val selectedGroups: Option[List[TimingWindow]] =
                  selObsSet
                    .flatMap: focusedObsIdSet =>
                      observations // All focused obs have the same constraints, so we can use head
                        .get(focusedObsIdSet.idSet.head)
                        .map(_.timingWindows)

                val obsAndConstraints: List[(Observation.Id, List[TimingWindow])] =
                  selectedGroups
                    .map: cs =>
                      copiedObsIdSet.idSet.toList.map: obsId =>
                        (obsId, cs)
                    .orEmpty

                IO.whenA(obsAndConstraints.nonEmpty):
                  applyObs(
                    obsAndConstraints,
                    props.programSummaries,
                    props.expandedIds
                  ).withToastDuring(
                    s"Pasting obs ${copiedObsIdSet.idSet.toList.mkString(", ")} into active scheduling group",
                    s"Pasted obs ${copiedObsIdSet.idSet.toList.mkString(", ")} into active scheduling group".some
                  )
              case _                                                 => IO.unit
            .runAsync
            .unless_(readonly)
      .useGlobalHotkeysWithDepsBy((_, _, _, copyCallback, pasteCallback) =>
        (copyCallback, pasteCallback)
      ): (props, ctx, _, _, _) =>
        (copyCallback, pasteCallback) =>
          def callbacks: ShortcutCallbacks =
            case CopyAlt1 | CopyAlt2   => copyCallback
            case PasteAlt1 | PasteAlt2 => pasteCallback
            case GoToSummary           =>
              ctx.setPageVia(
                (AppTab.Scheduling, props.programId, Focused.None).some,
                SetRouteVia.HistoryPush
              )

          UseHotkeysProps((GoToSummary :: (CopyKeys ::: PasteKeys)).toHotKeys, callbacks)
      .useStateView[SelectedPanel](SelectedPanel.Uninitialized)
      .useEffectWithDepsBy((props, _, _, _, _, state) => (props.focusedObsSet, state.reuseByValue)):
        (_, _, _, _, _, _) =>
          (focusedObsSet, selected) =>
            (focusedObsSet, selected.get) match
              case (Some(_), _)                 => selected.set(SelectedPanel.Editor)
              case (None, SelectedPanel.Editor) => selected.set(SelectedPanel.Summary)
              case _                            => Callback.empty
      .useResizeDetector() // Measure its size
      .render: (props, ctx, shadowClipboardObs, copyCallback, pasteCallback, state, resize) =>
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
            .flatMap: ids =>
              findSchedulingGroup(ids, props.programSummaries.get.schedulingGroups)
                .map(cg => (ids, cg))
            .fold[VdomNode] {
              <.div(LucumaStyles.HVCenter)(
                <.div("Select a scheduling group from the list.")
              )
            } { case (idsToEdit, schedulingGroup) =>
              val obsTraversal: Traversal[ObservationList, Observation] =
                Iso
                  .id[ObservationList]
                  .filterIndex((id: Observation.Id) => idsToEdit.contains(id))

              val twTraversal: Traversal[ObservationList, List[TimingWindow]] =
                obsTraversal.andThen(Observation.timingWindows)

              val timingWindows: View[List[TimingWindow]] =
                TimingWindowsQueries
                  .viewWithRemoteMod(
                    idsToEdit,
                    observations
                      .undoableView[List[TimingWindow]](
                        twTraversal.getAll.andThen(_.head),
                        twTraversal.modify
                      )
                  )

              val schedulingWindowsTile =
                SchedulingWindowsTile(timingWindows, props.readonly, true)

              TileController(
                props.userId,
                resize.width.getOrElse(1),
                ExploreGridLayouts.sectionLayout(GridLayoutSection.SchedulingLayout),
                props.userPreferences.schedulingTabLayout,
                List(schedulingWindowsTile),
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
            props.programSummaries.get.calibrationObservations,
            props.programSummaries.get.obsExecutionPots,
            props.focusedObsSet,
            state.set(SelectedPanel.Summary),
            props.expandedIds,
            copyCallback,
            pasteCallback,
            shadowClipboardObs.value,
            props.programSummaries.get.allocatedScienceBands,
            props.readonly
          )

        React.Fragment(
          if (LinkingInfo.developmentMode)
            FocusedStatus(AppTab.Scheduling, props.programId, Focused(props.focusedObsSet))
          else EmptyVdom,
          makeOneOrTwoPanels(state, schedulingTree, rightSide, RightSideCardinality.Single, resize)
        )
