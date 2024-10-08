// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.numeric.NonNegInt
import explore.*
import explore.Icons
import explore.components.ColumnSelectorInTitle
import explore.components.ColumnSelectorState
import explore.components.Tile
import explore.components.ToolbarTooltipOptions
import explore.components.ui.ExploreStyles
import explore.data.KeyedIndexedList
import explore.model.*
import explore.model.AppContext
import explore.model.GroupTree.syntax.*
import explore.model.Observation
import explore.model.ObservationExecutionMap
import explore.model.ProgramSummaries
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.reusability.given
import explore.modes.SpectroscopyModesMatrix
import explore.observationtree.*
import explore.shortcuts.*
import explore.shortcuts.given
import explore.syntax.ui.*
import explore.undo.UndoContext
import explore.undo.UndoSetter
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.NewType
import lucuma.react.common.*
import lucuma.react.hotkeys.*
import lucuma.react.hotkeys.hooks.*
import lucuma.react.primereact.Button
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.table.Expandable
import lucuma.refined.*
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import monocle.Iso

object DeckShown extends NewType[Boolean]:
  inline def Shown: DeckShown  = DeckShown(true)
  inline def Hidden: DeckShown = DeckShown(false)

  extension (s: DeckShown)
    def flip: DeckShown =
      if (s.value) DeckShown.Hidden else DeckShown.Shown

type DeckShown = DeckShown.Type

case class ObsTabContents(
  vault:            Option[UserVault],
  programId:        Program.Id,
  programSummaries: UndoContext[ProgramSummaries],
  userPreferences:  View[UserPreferences],
  modes:            SpectroscopyModesMatrix,
  focused:          Focused,
  searching:        View[Set[Target.Id]],
  expandedGroups:   View[Set[Group.Id]]
) extends ReactFnProps(ObsTabContents.component):
  private val focusedObs: Option[Observation.Id]                           = focused.obsSet.map(_.head)
  private val focusedTarget: Option[Target.Id]                             = focused.target
  private val focusedGroup: Option[Group.Id]                               = focused.group
  private val observations: UndoSetter[ObservationList]                    =
    programSummaries.zoom(ProgramSummaries.observations)
  private val groups: UndoSetter[GroupTree]                                = programSummaries.zoom(ProgramSummaries.groups)
  private val systemGroups: GroupTree                                      = programSummaries.get.systemGroups
  private val activeGroup: Option[Group.Id]                                = focusedGroup.orElse:
    focusedObs.flatMap(groups.get.obsGroupId)
  private val obsExecutions: ObservationExecutionMap                       = programSummaries.get.obsExecutionPots
  private val groupTimeRanges: GroupTimeRangeMap                           = programSummaries.get.groupTimeRangePots
  private val targets: UndoSetter[TargetList]                              = programSummaries.zoom(ProgramSummaries.targets)
  private val observationIdsWithIndices: List[(Observation.Id, NonNegInt)] =
    observations.get.toIndexedList.map((o, idx) => (o.id, idx))
  val readonly: Boolean                                                    = programSummaries.get.proposalIsSubmitted

object ObsTabContents extends TwoPanels:
  private type Props = ObsTabContents

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView[SelectedPanel](SelectedPanel.Uninitialized)
      .useEffectWithDepsBy((props, _, _) => props.focusedObs): (_, _, selected) =>
        focusedObs =>
          (focusedObs, selected.get) match
            case (Some(_), _)                 => selected.set(SelectedPanel.Editor)
            case (None, SelectedPanel.Editor) => selected.set(SelectedPanel.Summary)
            case _                            => Callback.empty
      .useResizeDetector() // Measure its size
      .useState(none[ObsIdSet]) // shadowClipboardObs (a copy as state only if it has observations)
      .useEffectOnMountBy: (_, ctx, _, _, shadowClipboardObs) => // initialize shadowClipboard
        import ctx.given

        ExploreClipboard.get.flatMap:
          _ match
            case LocalClipboard.CopiedObservations(idSet) =>
              shadowClipboardObs.setStateAsync(idSet.some)
            case _                                        => IO.unit
      .useCallbackWithDepsBy((props, _, _, _, _) => props.focusedObs): // COPY Action Callback
        (_, ctx, _, _, shadowClipboardObs) =>
          obs =>
            import ctx.given

            obs
              .map: id =>
                (ExploreClipboard
                  .set(LocalClipboard.CopiedObservations(ObsIdSet.one(id))) >>
                  shadowClipboardObs.setStateAsync(ObsIdSet.one(id).some))
                  .withToast(s"Copied obs $id")
              .orUnit
              .runAsync
      .useCallbackWithDepsBy((props, _, _, _, _, _) => // PASTE Action Callback
        (Reusable.explicitly(props.observations)(Reusability.by(_.get)),
         props.activeGroup,
         props.readonly
        )
      ): (props, ctx, _, _, _, _) =>
        (observations, activeGroup, readonly) =>
          import ctx.given

          ExploreClipboard.get
            .flatMap:
              case LocalClipboard.CopiedObservations(obsIdSet) =>
                obsIdSet.idSet.toList
                  .traverse: oid =>
                    cloneObs(props.programId, oid, activeGroup, observations, ctx)
                  .void
                  .withToast(s"Duplicating obs ${obsIdSet.idSet.mkString_(", ")}")
              case _                                           => IO.unit
            .runAsync
            .unless_(readonly)
      .useGlobalHotkeysWithDepsBy((props, _, _, _, _, copyCallback, pasteCallback) =>
        (copyCallback, pasteCallback, props.focusedObs, props.observationIdsWithIndices)
      ): (props, ctx, _, _, _, _, _) =>
        (copyCallback, pasteCallback, obs, obsIdsWithIndices) =>
          val obsPos: Option[NonNegInt] =
            obsIdsWithIndices.find(a => obs.forall(_ === a._1)).map(_._2)

          def callbacks: ShortcutCallbacks = {
            case CopyAlt1 | CopyAlt2 => copyCallback

            case PasteAlt1 | PasteAlt2 => pasteCallback

            case Down =>
              obsPos
                .filter(_.value < obsIdsWithIndices.length)
                .flatMap: p =>
                  val next = if (props.focusedObs.isEmpty) 0 else p.value + 1
                  obsIdsWithIndices
                    .lift(next)
                    .map: (obsId, _) =>
                      ctx.setPageVia(
                        AppTab.Observations,
                        props.programId,
                        Focused.singleObs(obsId),
                        SetRouteVia.HistoryPush
                      )
                .getOrEmpty

            case Up =>
              obsPos
                .filter(_.value > 0)
                .flatMap: p =>
                  obsIdsWithIndices
                    .lift(p.value - 1)
                    .map: (obsId, _) =>
                      ctx.setPageVia(
                        AppTab.Observations,
                        props.programId,
                        Focused.singleObs(obsId),
                        SetRouteVia.HistoryPush
                      )
                .getOrEmpty

            case GoToSummary =>
              ctx.setPageVia(
                AppTab.Observations,
                props.programId,
                Focused.None,
                SetRouteVia.HistoryPush
              )
          }
          UseHotkeysProps(
            ((GoToSummary :: Up :: Down :: Nil) ::: (CopyKeys ::: PasteKeys)).toHotKeys,
            callbacks
          )
      .useStateView(DeckShown.Shown)
      .render:
        (
          props,
          ctx,
          twoPanelState,
          resize,
          shadowClipboardObs,
          copyCallback,
          pasteCallback,
          deckShown
        ) =>

          val observationsTree: VdomNode =
            if (deckShown.get === DeckShown.Shown) {
              ObsList(
                props.observations,
                props.obsExecutions,
                props.programSummaries,
                props.programId,
                props.focusedObs,
                props.focusedTarget,
                props.focusedGroup,
                twoPanelState.set(SelectedPanel.Summary),
                props.groups,
                props.systemGroups,
                props.expandedGroups,
                deckShown,
                copyCallback,
                pasteCallback,
                shadowClipboardObs.value,
                props.programSummaries.get.allocatedScienceBands,
                props.readonly
              )
            } else
              <.div(ExploreStyles.TreeToolbar)(
                Button(
                  severity = Button.Severity.Secondary,
                  outlined = true,
                  disabled = false,
                  tooltip = "Show Observation Tree",
                  tooltipOptions = ToolbarTooltipOptions.Default,
                  icon = Icons.ArrowRightFromLine,
                  clazz = ExploreStyles.ObsTreeHideShow,
                  onClick = deckShown.mod(_.flip)
                ).mini.compact
              )

          val backButton: VdomNode =
            makeBackButton(props.programId, AppTab.Observations, twoPanelState, ctx)

          val observationTable: VdomNode =
            Tile(
              "observations".refined,
              "Observations Summary",
              ColumnSelectorState[Expandable[ObsSummaryTable.ObsSummaryRow], Nothing](),
              backButton.some,
              canMinimize = false,
              canMaximize = false
            )(
              ObsSummaryTable(
                props.vault.userId,
                props.programId,
                props.observations,
                props.groups.model,
                props.obsExecutions,
                props.targets.get,
                props.programSummaries.get.allocatedScienceBands.size > 1,
                _
              ),
              (s, _) => ColumnSelectorInTitle(ObsSummaryTable.selectableColumnNames.get, s)
              // TODO: asterism elevation view
            )

          def obsTiles(obsId: Observation.Id, resize: UseResizeDetectorReturn): VdomNode =
            val indexValue = Iso.id[ObservationList].index(obsId).andThen(KeyedIndexedList.value)

            props.observations.model
              .zoom(indexValue)
              .mapValue(obsView =>
                ObsTabTiles(
                  props.vault,
                  props.programId,
                  props.modes,
                  backButton,
                  // FIXME Find a better mechanism for this.
                  // Something like .mapValue but for UndoContext
                  props.observations.zoom(indexValue.getOption.andThen(_.get), indexValue.modify),
                  props.programSummaries
                    .zoom((ProgramSummaries.observations, ProgramSummaries.targets).disjointZip),
                  props.programSummaries.model.zoom(ProgramSummaries.obsAttachments),
                  props.programSummaries.get,
                  props.focusedTarget,
                  props.searching,
                  // We need this as a separate view so it doesn't get in the way of undo and can be easily updated by AGS
                  obsView.zoom(Observation.selectedGSName),
                  ExploreGridLayouts.sectionLayout(GridLayoutSection.ObservationsLayout),
                  props.userPreferences.get.observationsTabLayout,
                  resize,
                  props.userPreferences.zoom(UserPreferences.globalPreferences),
                  props.readonly
                ).withKey(s"${obsId.show}")
              )

          def groupTiles(groupId: Group.Id, resize: UseResizeDetectorReturn): VdomNode =
            ObsGroupTiles(
              props.vault.userId,
              groupId,
              props.groups,
              props.groupTimeRanges.getPot(groupId),
              resize,
              ExploreGridLayouts.sectionLayout(GridLayoutSection.GroupEditLayout),
              props.userPreferences.get.groupEditLayout,
              backButton
            )

          def rightSide(resize: UseResizeDetectorReturn): VdomNode =
            (props.focusedObs, props.focusedGroup) match
              case (Some(obsId), _)   => obsTiles(obsId, resize)
              case (_, Some(groupId)) => groupTiles(groupId, resize)
              case _                  => observationTable

          makeOneOrTwoPanels(
            twoPanelState,
            observationsTree,
            rightSide,
            RightSideCardinality.Multi,
            resize,
            ExploreStyles.ObsHiddenToolbar.when_(deckShown.get === DeckShown.Hidden)
          )
