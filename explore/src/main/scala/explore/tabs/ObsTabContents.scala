// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.components.Tile
import explore.components.TileController
import explore.components.ToolbarTooltipOptions
import explore.components.ui.ExploreStyles
import explore.model.*
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.reusability.given
import explore.modes.SpectroscopyModesMatrix
import explore.observationtree.*
import explore.plots.ElevationPlotTile
import explore.plots.ObjectPlotData
import explore.plots.PlotData
import explore.shortcuts.*
import explore.shortcuts.given
import explore.syntax.ui.*
import explore.undo.UndoContext
import explore.undo.UndoSetter
import explore.undo.Undoer
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.NewBoolean
import lucuma.react.common.*
import lucuma.react.hotkeys.*
import lucuma.react.hotkeys.hooks.*
import lucuma.react.primereact.Button
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import monocle.Iso
import monocle.Optional

object DeckShown extends NewBoolean:
  inline def Shown                             = True
  inline def Hidden                            = False
  extension (s: DeckShown) def flip: DeckShown = if s then DeckShown.Hidden else DeckShown.Shown

type DeckShown = DeckShown.Type

case class ObsTabContents(
  vault:            Option[UserVault],
  programId:        Program.Id,
  programSummaries: UndoContext[ProgramSummaries],
  userPreferences:  View[UserPreferences],
  modes:            SpectroscopyModesMatrix,
  focused:          Focused,
  searching:        View[Set[Target.Id]],
  expandedGroups:   View[Set[Group.Id]],
  readonly:         Boolean
) extends ReactFnProps(ObsTabContents.component):
  private val focusedObs: Option[Observation.Id]         = focused.obsSet.map(_.head)
  private val focusedTarget: Option[Target.Id]           = focused.target
  private val focusedGroup: Option[Group.Id]             = focused.group
  private val observations: UndoSetter[ObservationList]  =
    programSummaries.zoom(ProgramSummaries.observations)
  private val groups: UndoSetter[GroupList]              = programSummaries.zoom(ProgramSummaries.groups)
  private val activeGroup: Option[Group.Id]              = focusedGroup.orElse:
    focusedObs.flatMap(observations.get.get(_)).flatMap(_.groupId)
  private val obsExecutions: ObservationExecutionMap     = programSummaries.get.obsExecutionPots
  private val groupTimeRanges: GroupTimeRangeMap         = programSummaries.get.groupTimeRangePots
  private val targets: UndoSetter[TargetList]            = programSummaries.zoom(ProgramSummaries.targets)
  private val globalPreferences: View[GlobalPreferences] =
    userPreferences.zoom(UserPreferences.globalPreferences)

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
      .useState(none[ObsIdSet])                 // shadowClipboardObs (a copy as state only if it has observations)
      .useEffectOnMountBy: (_, ctx, _, _, shadowClipboardObs) => // initialize shadowClipboard
        import ctx.given

        ExploreClipboard.get.flatMap:
          _ match
            case LocalClipboard.CopiedObservations(idSet) =>
              shadowClipboardObs.setStateAsync(idSet.some)
            case _                                        => IO.unit
      .useStateView(List.empty[Observation.Id]) // selectedObsIds
      .localValBy: (props, _, _, _, _, selectedObsIds) => // selectedOrFocusedObsIds
        props.focusedObs.map(ObsIdSet.one(_)).orElse(ObsIdSet.fromList(selectedObsIds.get))
      .useCallbackWithDepsBy((_, _, _, _, _, _, selectedOrFocusedObsIds) =>
        selectedOrFocusedObsIds
      ): // COPY Action Callback
        (_, ctx, _, _, shadowClipboardObs, _, _) =>
          selectedOrFocusedObsIds =>
            import ctx.given

            selectedOrFocusedObsIds
              .map: obsIdSet =>
                (ExploreClipboard
                  .set(LocalClipboard.CopiedObservations(obsIdSet)) >>
                  shadowClipboardObs.setStateAsync(obsIdSet.some))
                  .withToast(s"Copied observation(s) ${obsIdSet.idSet.toList.mkString(", ")}")
              .orUnit
              .runAsync
      .useCallbackWithDepsBy((props, _, _, _, _, _, _, _) => // PASTE Action Callback
        (Reusable.explicitly(props.observations)(Reusability.by(_.get)),
         props.activeGroup,
         props.readonly
        )
      ): (props, ctx, _, _, _, _, _, _) =>
        (observations, activeGroup, readonly) =>
          import ctx.given

          ExploreClipboard.get
            .flatMap:
              case LocalClipboard.CopiedObservations(obsIdSet) =>
                cloneObs(
                  props.programId,
                  obsIdSet.idSet.toList,
                  activeGroup,
                  observations,
                  ctx
                ).withToastDuring(s"Duplicating obs ${obsIdSet.idSet.mkString_(", ")}")
              case _                                           => IO.unit
            .runAsync
            .unless_(readonly)
      .useGlobalHotkeysWithDepsBy((props, _, _, _, _, _, _, copyCallback, pasteCallback) =>
        (copyCallback, pasteCallback)
      ): (props, ctx, _, _, _, _, _, _, _) =>
        (copyCallback, pasteCallback) =>
          def callbacks: ShortcutCallbacks = {
            case CopyAlt1 | CopyAlt2 => copyCallback

            case PasteAlt1 | PasteAlt2 => pasteCallback

            case GoToSummary =>
              ctx.setPageVia(
                (AppTab.Observations, props.programId, Focused.None).some,
                SetRouteVia.HistoryPush
              )
          }
          UseHotkeysProps(
            ((GoToSummary :: Up :: Down :: Nil) ::: (CopyKeys ::: PasteKeys)).toHotKeys,
            callbacks
          )
      .useStateView(DeckShown.Shown)
      .useStateView(AddingObservation(false))   // adding new observation or duplicating
      .render:
        (
          props,
          ctx,
          twoPanelState,
          resize,
          shadowClipboardObs,
          selectedObsIds,
          selectedOrFocusedObsIds, // Mixes focused obs and selected obs in table
          copyCallback,
          pasteCallback,
          deckShown,
          addingObservation
        ) =>
          val observationsTree: VdomNode =
            if (deckShown.get === DeckShown.Shown) {
              ObsTree(
                props.programId,
                props.observations,
                props.groups,
                props.programSummaries.get.groupsChildren,
                props.programSummaries.get.parentGroups(_),
                props.programSummaries.get.groupWarnings,
                props.obsExecutions,
                props.programSummaries: Undoer,
                props.focusedObs,
                props.focusedTarget,
                props.focusedGroup,
                selectedObsIds.get,
                twoPanelState.set(SelectedPanel.Summary),
                props.expandedGroups,
                deckShown,
                copyCallback,
                pasteCallback,
                shadowClipboardObs.value,
                props.programSummaries.get.allocatedScienceBands,
                addingObservation,
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

          val obsSummaryTableTile: Tile[?] =
            ObsSummaryTile(
              props.vault.userId,
              props.programId,
              props.observations,
              selectedObsIds,
              props.groups.model,
              props.obsExecutions,
              props.targets.get,
              props.programSummaries.get.allocatedScienceBands.size > 1,
              backButton
            )

          val plotData: PlotData =
            PlotData:
              selectedOrFocusedObsIds
                .foldMap(_.idSet.toList)
                .map(props.observations.get.get(_))
                .flattenOption
                .map: obs =>
                  obs
                    .asterismTracking(props.programSummaries.get.targets)
                    .map: tracking =>
                      ObjectPlotData.Id(obs.id.asLeft) ->
                        ObjectPlotData(
                          NonEmptyString.unsafeFrom(s"${obs.title} (${obs.id.toString})"),
                          tracking,
                          obs.basicConfiguration.foldMap(conf => List(conf.siteFor))
                        )
                .flattenOption
                .toMap

          val skyPlotTile: Tile[?] =
            ElevationPlotTile(
              props.vault.userId,
              ObsSummaryTabTileIds.PlotId.id,
              plotData,
              none, // TODO Deduce site from the first target?
              none,
              none,
              List.empty,
              props.globalPreferences.get,
              "No observation selected"
            )

          val summaryTiles: VdomNode =
            TileController(
              props.vault.map(_.user.id),
              resize.width.getOrElse(0),
              ExploreGridLayouts.sectionLayout(GridLayoutSection.ObservationListLayout),
              props.userPreferences.get.observationListTabLayout,
              List(obsSummaryTableTile, skyPlotTile),
              GridLayoutSection.ObservationListLayout
            )

          def obsEditorTiles(obsId: Observation.Id, resize: UseResizeDetectorReturn): VdomNode = {
            val indexValue: Optional[ObservationList, Observation] =
              Iso.id[ObservationList].index(obsId)

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
                  props.programSummaries.model.zoom(ProgramSummaries.attachments),
                  props.programSummaries.get,
                  props.focusedTarget,
                  props.searching,
                  // We need this as a separate view so it doesn't get in the way of undo and can be easily updated by AGS
                  obsView.zoom(Observation.selectedGSName),
                  resize,
                  props.userPreferences.get,
                  props.globalPreferences,
                  props.readonly || addingObservation.get.value
                ).withKey(s"${obsId.show}")
              )
          }

          def groupEditorTiles(groupId: Group.Id, resize: UseResizeDetectorReturn): VdomNode =
            props.groups
              .zoom(Iso.id[GroupList].index(groupId))
              .map: group =>
                ObsGroupTiles(
                  props.vault.userId,
                  group,
                  props.programSummaries.get.groupWarnings.get(group.get.id),
                  props.programSummaries.get.groupsChildren.get(groupId.some).map(_.length).orEmpty,
                  props.groupTimeRanges.getPot(groupId),
                  resize,
                  ExploreGridLayouts.sectionLayout(GridLayoutSection.GroupEditLayout),
                  props.userPreferences.get.groupEditLayout,
                  backButton
                )

          def rightSide(resize: UseResizeDetectorReturn): VdomNode =
            (props.focusedObs, props.focusedGroup) match
              case (Some(obsId), _)   => obsEditorTiles(obsId, resize)
              case (_, Some(groupId)) => groupEditorTiles(groupId, resize)
              case _                  => summaryTiles

          makeOneOrTwoPanels(
            twoPanelState,
            observationsTree,
            rightSide,
            RightSideCardinality.Multi,
            resize,
            ExploreStyles.ObsHiddenToolbar.when_(deckShown.get === DeckShown.Hidden)
          )
