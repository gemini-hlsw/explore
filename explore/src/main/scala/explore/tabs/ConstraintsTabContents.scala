// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import explore.*
import explore.actions.ObservationPasteIntoConstraintSetAction
import explore.common.TimingWindowsQueries
import explore.components.FocusedStatus
import explore.components.Tile
import explore.components.TileController
import explore.constraints.ConstraintsPanel
import explore.constraints.ConstraintsSummaryTile
import explore.model.*
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.reusability.given
import explore.observationtree.ConstraintGroupObsList
import explore.schedulingWindows.SchedulingWindowsTile
import explore.shortcuts.*
import explore.shortcuts.given
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.callback.CallbackCatsEffect.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ConstraintSet
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

case class ConstraintsTabContents(
  userId:           Option[User.Id],
  programId:        Program.Id,
  programSummaries: UndoContext[ProgramSummaries],
  userPreferences:  UserPreferences,
  focusedObsSet:    Option[ObsIdSet],
  expandedIds:      View[SortedSet[ObsIdSet]],
  readonly:         Boolean
) extends ReactFnProps(ConstraintsTabContents.component):
  private val observations: ObservationList =
    programSummaries.zoom(ProgramSummaries.observations).get

object ConstraintsTabContents extends TwoPanels:
  private type Props = ConstraintsTabContents

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
                val selectedConstraints: Option[ConstraintSet] =
                  selObsSet
                    .flatMap: focusedObsIdSet =>
                      observations // All focused obs have the same constraints, so we can use head
                        .get(focusedObsIdSet.idSet.head)
                        .map(_.constraints)

                val obsAndConstraints: List[(Observation.Id, ConstraintSet)] =
                  selectedConstraints
                    .map: cs =>
                      copiedObsIdSet.idSet.toList.map: obsId =>
                        (obsId, cs)
                    .orEmpty

                IO.whenA(obsAndConstraints.nonEmpty):
                  ObservationPasteIntoConstraintSetAction(
                    obsAndConstraints,
                    props.expandedIds.async.mod
                  )(props.programSummaries).void
                    .withToastDuring(
                      s"Pasting obs ${copiedObsIdSet.idSet.toList.mkString(", ")} into active constraint set",
                      s"Pasted obs ${copiedObsIdSet.idSet.toList.mkString(", ")} into active constraint set".some
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
                (AppTab.Constraints, props.programId, Focused.None).some,
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

        def findConstraintGroup(
          obsIds: ObsIdSet,
          cgl:    ConstraintGroupList
        ): Option[ConstraintGroup] =
          cgl.find(_._1.intersect(obsIds).nonEmpty).map(ConstraintGroup.fromTuple)

        val backButton: VdomNode =
          makeBackButton(props.programId, AppTab.Constraints, state, ctx)

        val observations: UndoSetter[ObservationList] =
          props.programSummaries.zoom(ProgramSummaries.observations)

        val rightSide = (_: UseResizeDetectorReturn) =>
          props.focusedObsSet
            .flatMap: ids =>
              findConstraintGroup(ids, props.programSummaries.get.constraintGroups)
                .map(cg => (ids, cg))
            .fold[VdomNode](
              ConstraintsSummaryTile(
                props.userId,
                props.programId,
                props.programSummaries.get.constraintGroups,
                props.expandedIds,
                backButton
              )
            ) { case (idsToEdit, constraintGroup) =>
              val obsTraversal = Iso
                .id[ObservationList]
                .filterIndex((id: Observation.Id) => idsToEdit.contains(id))

              val csTraversal = obsTraversal.andThen(Observation.constraints)

              val constraintSet: UndoSetter[ConstraintSet] =
                observations.zoom(csTraversal.getAll.andThen(_.head), csTraversal.modify)

              val constraintsTitle: String = idsToEdit.single match
                case Some(id) => s"Observation $id"
                case None     => s"Editing Constraints for ${idsToEdit.size} Observations"

              val constraintsTile: Tile[Option[VdomNode]] =
                Tile(
                  ObsTabTileIds.ConstraintsId.id,
                  constraintsTitle,
                  backButton.some
                )(_ => ConstraintsPanel(idsToEdit, none, none, none, constraintSet, props.readonly))

              val twTraversal = obsTraversal.andThen(Observation.timingWindows)

              val timingWindows: View[List[TimingWindow]] =
                TimingWindowsQueries.viewWithRemoteMod(
                  idsToEdit,
                  observations
                    .undoableView[List[TimingWindow]](
                      twTraversal.getAll.andThen(_.head),
                      twTraversal.modify
                    )
                )

              val schedulingWindowsTile: Tile[SchedulingWindowsTile.TileState] =
                SchedulingWindowsTile(timingWindows, props.readonly, false)

              TileController(
                props.userId,
                resize.width.getOrElse(1),
                ExploreGridLayouts.sectionLayout(GridLayoutSection.ConstraintsLayout),
                props.userPreferences.constraintsTabLayout,
                List(constraintsTile, schedulingWindowsTile),
                GridLayoutSection.ConstraintsLayout,
                None
              )
            }

        val constraintsTree: VdomNode =
          ConstraintGroupObsList(
            props.programId,
            observations,
            props.programSummaries,
            props.programSummaries.get.constraintGroups,
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
            FocusedStatus(AppTab.Constraints, props.programId, Focused(props.focusedObsSet))
          else EmptyVdom,
          makeOneOrTwoPanels(state, constraintsTree, rightSide, RightSideCardinality.Multi, resize)
        )
