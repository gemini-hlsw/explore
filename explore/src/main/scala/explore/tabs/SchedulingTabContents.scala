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
import explore.actions.ObservationPasteIntoSchedulingGroupAction
import explore.components.FocusedStatus
import explore.components.TileController
import explore.model.*
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
import lucuma.ui.LucumaStyles
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given

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

  private val component =
    ScalaFnComponent[Props]: props =>
      for {
        ctx                <- useContext(AppContext.ctx)
        // shadowClipboardObs (a copy as state only if it has observations)
        shadowClipboardObs <- useState(none[ObsIdSet])
        // initialize shadowClipboard
        _                  <- useEffectOnMount:
                                import ctx.given

                                ExploreClipboard.get.flatMap:
                                  _ match
                                    case LocalClipboard.CopiedObservations(idSet) =>
                                      shadowClipboardObs.setStateAsync(idSet.some)
                                    case _                                        => IO.unit
        // COPY Action Callback
        copyCallback       <- useCallbackWithDeps(props.focusedObsSet): focusedObsSet =>
                                import ctx.given

                                focusedObsSet
                                  .map: obsIdSet =>
                                    (ExploreClipboard
                                      .set(LocalClipboard.CopiedObservations(obsIdSet)) >>
                                      shadowClipboardObs.setStateAsync(obsIdSet.some))
                                      .withToast(
                                        s"Copied observation(s) ${obsIdSet.idSet.toList.mkString(", ")}"
                                      )
                                  .orUnit
                                  .runAsync
        // PASTE Action Callback
        pasteCallback      <-
          useCallbackWithDeps((props.observations, props.focusedObsSet, props.readonly)):
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
                      ObservationPasteIntoSchedulingGroupAction(
                        obsAndConstraints,
                        props.expandedIds.async.mod
                      )(props.programSummaries).void
                        .withToastDuring(
                          s"Pasting obs ${copiedObsIdSet.idSet.toList.mkString(", ")} into active scheduling group",
                          s"Pasted obs ${copiedObsIdSet.idSet.toList.mkString(", ")} into active scheduling group".some
                        )
                  case _                                                 => IO.unit
                .runAsync
                .unless_(readonly)
        _                  <- useGlobalHotkeysWithDeps((copyCallback, pasteCallback)):
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
        state              <- useStateView[SelectedPanel](SelectedPanel.Uninitialized)
        _                  <- useEffectWithDeps((props.focusedObsSet, state.reuseByValue)):
                                (focusedObsSet, selected) =>
                                  (focusedObsSet, selected.get) match
                                    case (Some(_), _)                 => selected.set(SelectedPanel.Editor)
                                    case (None, SelectedPanel.Editor) => selected.set(SelectedPanel.Summary)
                                    case _                            => Callback.empty
        obsEditInfo        <- useMemo((props.focusedObsSet, props.observations)): (focused, obsList) =>
                                focused.map(ObsIdSetEditInfo.fromObservationList(_, obsList))
        resize             <- useResizeDetector // Measure its size
      } yield
        import ctx.given

        def findSchedulingGroup(
          obsIds: ObsIdSet,
          cgl:    SchedulingGroupList
        ): Option[SchedulingGroup] =
          cgl.find(_._1.intersect(obsIds).nonEmpty).map(SchedulingGroup.fromTuple)

        val observations: UndoSetter[ObservationList] =
          props.programSummaries.zoom(ProgramSummaries.observations)

        val rightSide = (_: UseResizeDetectorReturn) =>
          obsEditInfo.value
            .flatMap: editInfo =>
              findSchedulingGroup(editInfo.editing, props.programSummaries.get.schedulingGroups)
                .map(cg => (editInfo, cg))
            .fold[VdomNode] {
              <.div(LucumaStyles.HVCenter)(
                <.div("Select a scheduling group from the list.")
              )
            } { case (obsEditInfo, schedulingGroup) =>
              val schedulingWindowsTile =
                SchedulingWindowsTile.forObsIdSet(obsEditInfo, observations, props.readonly, true)

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
          makeOneOrTwoPanels(
            state,
            schedulingTree,
            rightSide,
            RightSideCardinality.Single,
            resize
          )
        )
