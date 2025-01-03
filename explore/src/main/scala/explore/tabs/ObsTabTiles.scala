// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.Order.given
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.*
import crystal.Pot.Ready
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.common.TimingWindowsQueries
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationTile
import explore.config.sequence.SequenceTile
import explore.constraints.ConstraintsPanel
import explore.findercharts.FinderChartsTile
import explore.itc.ItcGraphQuerier
import explore.itc.ItcTile
import explore.model.*
import explore.model.AppContext
import explore.model.GuideStarSelection.*
import explore.model.Observation
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.model.OnCloneParameters
import explore.model.ProgramSummaries
import explore.model.TargetEditObsInfo
import explore.model.display.given
import explore.model.enums.AgsState
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.extensions.*
import explore.model.layout.*
import explore.modes.SpectroscopyModesMatrix
import explore.observationtree.obsEditAttachments
import explore.plots.ElevationPlotTile
import explore.plots.ObjectPlotData
import explore.plots.PlotData
import explore.schedulingWindows.SchedulingWindowsTile
import explore.syntax.ui.*
import explore.targeteditor.AsterismEditorTile
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.skycalc.averageParallacticAngle
import lucuma.core.model.ConstraintSet
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.ObjectTracking
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.syntax.all.*
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Dropdown
import lucuma.react.primereact.SelectItem
import lucuma.react.resizeDetector.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.odb.input.*
import lucuma.ui.optics.*
import lucuma.ui.reusability.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.common.ObsQueriesGQL.*
import queries.schemas.odb.ObsQueries

import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

case class ObsTabTiles(
  vault:             Option[UserVault],
  programId:         Program.Id,
  modes:             SpectroscopyModesMatrix,
  backButton:        VdomNode,
  observation:       UndoSetter[Observation],
  obsAndTargets:     UndoSetter[ObservationsAndTargets],
  attachments:       View[AttachmentList],
  programSummaries:  ProgramSummaries,
  focusedTarget:     Option[Target.Id],
  searching:         View[Set[Target.Id]],
  selectedGSName:    View[Option[NonEmptyString]],
  resize:            UseResizeDetectorReturn,
  userPreferences:   UserPreferences,
  globalPreferences: View[GlobalPreferences],
  readonly:          Boolean
) extends ReactFnProps(ObsTabTiles.component):
  val obsId: Observation.Id                                         = observation.get.id
  val isDisabled: Boolean                                           = readonly || observation.get.isCalibration
  val allConstraintSets: Set[ConstraintSet]                         = programSummaries.constraintGroups.map(_._2).toSet
  val targetObservations: Map[Target.Id, SortedSet[Observation.Id]] =
    programSummaries.targetObservations
  val obsExecution: Pot[Execution]                                  = programSummaries.obsExecutionPots.getPot(obsId)
  val obsTargets: TargetList                                        = programSummaries.obsTargets.get(obsId).getOrElse(SortedMap.empty)
  val obsAttachmentAssignments: ObsAttachmentAssignmentMap          =
    programSummaries.obsAttachmentAssignments
  val asterismTracking: Option[ObjectTracking]                      =
    observation.get.asterismTracking(obsTargets)
  val posAngleConstraint: PosAngleConstraint                        = observation.get.posAngleConstraint
  val calibrationRole: Option[CalibrationRole]                      = observation.zoom(Observation.calibrationRole).get

object ObsTabTiles:
  private type Props = ObsTabTiles

  private def makeConstraintsSelector(
    observationId:     Observation.Id,
    constraintSet:     View[ConstraintSet],
    allConstraintSets: Set[ConstraintSet],
    isDisabled:        Boolean
  )(using FetchClient[IO, ObservationDB]): VdomNode =
    <.div(ExploreStyles.TileTitleConstraintSelector)(
      Dropdown[ConstraintSet](
        value = constraintSet.get,
        disabled = isDisabled,
        onChange = (cs: ConstraintSet) =>
          constraintSet.set(cs) >>
            ObsQueries
              .updateObservationConstraintSet[IO](List(observationId), cs)
              .runAsyncAndForget,
        options = allConstraintSets
          .map(cs => new SelectItem[ConstraintSet](value = cs, label = cs.shortName))
          .toList
      )
    )

  private case class Offsets(
    science:     Option[NonEmptyList[Offset]],
    acquisition: Option[NonEmptyList[Offset]]
  )

  def roleLayout(
    userPreferences: UserPreferences,
    calibrationRole: Option[CalibrationRole]
  ): (GridLayoutSection, LayoutsMap, LayoutsMap) =
    def result(section: GridLayoutSection) =
      (section,
       ExploreGridLayouts.sectionLayout(section),
       UserPreferences.gridLayouts
         .index(section)
         .getOption(userPreferences)
         .getOrElse(ExploreGridLayouts.sectionLayout(section))
      )

    calibrationRole match
      case Some(CalibrationRole.SpectroPhotometric) =>
        result(GridLayoutSection.ObservationsSpecPhotoLayout)
      case Some(CalibrationRole.Twilight)           =>
        result(GridLayoutSection.ObservationsTwilightLayout)
      case _                                        =>
        result(GridLayoutSection.ObservationsLayout)

  // TODO Move to core
  extension (om: ObservingMode)
    def centralWavelength: Option[CentralWavelength] =
      ObservingMode.gmosNorthLongSlit
        .andThen(ObservingMode.GmosNorthLongSlit.centralWavelength)
        .getOption(om)
        .orElse(
          ObservingMode.gmosSouthLongSlit
            .andThen(ObservingMode.GmosSouthLongSlit.centralWavelength)
            .getOption(om)
        )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStreamResourceOnMountBy: (props, ctx) =>
        import ctx.given

        SequenceOffsets[IO]
          .query(props.obsId)
          .map: data =>
            Offsets(
              science = NonEmptyList.fromList(
                data.observation
                  .foldMap(_.execution.scienceOffsets)
                  .distinct
              ),
              acquisition = NonEmptyList.fromList(
                data.observation
                  .foldMap(_.execution.acquisitionOffsets)
                  .distinct
              )
            )
          // TODO Could we get the edit signal from ProgramCache instead of doing another subscritpion??
          .reRunOnResourceSignals:
            ObservationEditSubscription.subscribe[IO](props.obsId.toObservationEditInput)
      // Ags state
      .useStateView[AgsState](AgsState.Idle)
      // the configuration the user has selected from the spectroscopy modes table, if any
      .useStateView(none[InstrumentConfigAndItcResult])
      .localValBy: (props, _, _, _, selectedConfig) => // itcGraphQuerier
        ItcGraphQuerier(props.observation.get, selectedConfig.get, props.obsTargets)
      .useEffectResultWithDepsBy((_, _, _, _, _, itcGraphQuerier) => itcGraphQuerier):
        (_, ctx, _, _, _, _) =>
          itcGraphQuerier => // Compute ITC graph
            import ctx.given
            itcGraphQuerier.requestGraphs
      .useStateView(().ready) // Signal that the sequence has changed
      .useEffectKeepResultWithDepsBy((p, _, _, _, _, _, _, _) =>
        p.observation.model.get.observationTime
      ): (_, _, _, _, _, _, _, _) =>
        vizTime => IO(vizTime.getOrElse(Instant.now()))
      // Store guide star selection in a view for fast local updates
      // This is not the ideal place for this but we need to share the selected guide star
      // across the configuration and target tile
      .useStateViewBy: (props, _, _, _, _, _, _, _, _) =>
        props.selectedGSName.get.fold(GuideStarSelection.Default)(RemoteGSSelection.apply)
      .localValBy: (props, ctx, _, _, _, _, _, _, _, guideStarSelection) =>
        import ctx.given

        // We tell the backend and the local cache of changes to the selected guidestar
        // In some cases when we do a real override
        guideStarSelection.withOnMod {
          (_, _) match {
            // Change of override
            case (AgsOverride(m, _, _), AgsOverride(n, _, _)) if m =!= n =>
              props.selectedGSName.set(n.some) *>
                ObsQueries
                  .setGuideTargetName[IO](props.obsId, n.some)
                  .runAsyncAndForget
            // Going from automatic to manual selection
            case (AgsSelection(_), AgsOverride(n, _, _))                 =>
              props.selectedGSName.set(n.some) *>
                ObsQueries
                  .setGuideTargetName[IO](props.obsId, n.some)
                  .runAsyncAndForget
            // Going from manual to automated selection
            case (AgsOverride(n, _, _), AgsSelection(_))                 =>
              props.selectedGSName.set(none) *>
                ObsQueries
                  .setGuideTargetName[IO](props.obsId, none)
                  .runAsyncAndForget
            case _                                                       =>
              // All other combinations
              Callback.empty
          }
        }
      .useStateBy((p, _, _, _, _, _, _, _, _, _, _) =>
        roleLayout(p.userPreferences, p.calibrationRole)
      )
      .useEffectWithDepsBy((p, _, _, _, _, _, _, _, _, _, _, _) => p.calibrationRole):
        (p, _, _, _, _, _, _, _, _, _, _, l) =>
          role => l.setState(roleLayout(p.userPreferences, role))
      .render:
        (
          props,
          ctx,
          sequenceOffsets,
          agsState,
          selectedConfig,
          itcGraphQuerier,
          itcGraphResults,
          sequenceChanged,
          vizTimeOrNowPot,
          _,
          guideStarSelection,
          roleLayouts
        ) =>
          import ctx.given
          val (section, defaultLayout, layout) = roleLayouts.value

          vizTimeOrNowPot.renderPot: vizTimeOrNow =>

            val asterismIds: View[AsterismIds] =
              props.observation.model.zoom(Observation.scienceTargetIds)

            val basicConfiguration: Option[BasicConfiguration] =
              props.observation.get.observingMode.map(_.toBasicConfiguration)

            val obsTimeView: View[Option[Instant]] =
              props.observation.model.zoom(Observation.observationTime)

            val obsDurationView: View[Option[TimeSpan]] =
              props.observation.model.zoom(Observation.observationDuration)

            val asterismAsNel: Option[NonEmptyList[TargetWithId]] =
              NonEmptyList.fromList:
                props.obsTargets.toList.map((tid, t) => TargetWithId(tid, t))

            // asterism base coordinates at viz time or current time
            val targetCoords: Option[CoordinatesAtVizTime] =
              asterismAsNel
                .flatMap(asterismNel => asterismNel.baseTracking.at(vizTimeOrNow))

            val attachmentsView =
              props.observation.model.zoom(Observation.attachmentIds).withOnMod { ids =>
                obsEditAttachments(props.obsId, ids).runAsync
              }

            val pendingTime = props.obsExecution.toOption.flatMap(_.programTimeEstimate)
            val obsDuration =
              props.observation.get.observationDuration
                .orElse(pendingTime)

            val paProps: PAProperties =
              PAProperties(props.obsId, guideStarSelection, agsState, props.posAngleConstraint)

            val averagePA: Option[AveragePABasis] =
              (basicConfiguration.map(_.siteFor),
               asterismAsNel,
               obsDuration.filter(_ > TimeSpan.Zero)
              )
                .mapN: (site, asterism, duration) =>
                  props.posAngleConstraint match
                    case PosAngleConstraint.AverageParallactic =>
                      // See also `anglesToTestAt` in AladinCell.scala.
                      averageParallacticAngle(
                        site.place,
                        asterism.baseTracking,
                        vizTimeOrNow,
                        duration
                      ).map(AveragePABasis(vizTimeOrNow, duration, _))
                    case _                                     => none
                .flatten

            // The angle used for `Align to PA` in the finder charts tile.
            // For Unbounded, use the PA of the currently selected guide star (if any)
            // For AverageParllactic constraint, use the average PA (if any), otherwise
            // use the angle specified in the constraint
            val pa: Option[Angle] =
              props.posAngleConstraint match
                case PosAngleConstraint.Unbounded                  => paProps.selectedPA
                case PosAngleConstraint.AverageParallactic         => averagePA.map(_.averagePA)
                case PosAngleConstraint.Fixed(angle)               => angle.some
                case PosAngleConstraint.AllowFlip(angle)           => angle.some
                case PosAngleConstraint.ParallacticOverride(angle) => angle.some

            // Only show finder charts and notes tiles if the proposal has been
            // accepted. Need to have hidden dummies in their place to not mess
            // up the stored layouts.
            val finderChartsTile =
              if (props.programSummaries.proposalIsAccepted)
                FinderChartsTile(
                  props.programId,
                  props.obsId,
                  attachmentsView,
                  props.vault.map(_.token),
                  props.attachments,
                  pa,
                  props.isDisabled
                )
              else Tile(ObsTabTileIds.FinderChartsId.id, "", hidden = true)(_ => EmptyVdom)

            val notesView: View[Option[NonEmptyString]] =
              props.observation.model
                .zoom(Observation.observerNotes)
                .withOnMod: notes =>
                  ObsQueries
                    .updateNotes[IO](List(props.obsId), notes)
                    .runAsync

            val notesTile =
              if (props.programSummaries.proposalIsAccepted)
                NotesTile.notesTile(props.obsId, notesView)
              else Tile(ObsTabTileIds.NotesId.id, "", hidden = true)(_ => EmptyVdom)

            val sequenceTile =
              SequenceTile(
                props.programId,
                props.obsId,
                props.obsExecution,
                asterismIds.get,
                sequenceChanged
              )

            val itcTile =
              ItcTile(
                props.vault.userId,
                props.obsId,
                props.obsTargets,
                itcGraphQuerier,
                itcGraphResults,
                props.globalPreferences
              )

            val constraints: View[ConstraintSet] =
              props.observation.model.zoom(Observation.constraints)

            val schedulingWindows: View[List[TimingWindow]] =
              TimingWindowsQueries.viewWithRemoteMod(
                ObsIdSet.one(props.obsId),
                props.observation.undoableView[List[TimingWindow]](Observation.timingWindows)
              )

            val obsConf: ObsConfiguration =
              ObsConfiguration(
                basicConfiguration,
                paProps.some,
                constraints.get.some,
                props.observation.get.observingMode.flatMap(_.centralWavelength),
                sequenceOffsets.toOption.flatMap(_.science),
                sequenceOffsets.toOption.flatMap(_.acquisition),
                averagePA,
                obsDuration.map(_.toDuration),
                props.observation.get.needsAGS,
                props.observation.get.selectedGSName,
                props.observation.get.calibrationRole
              )

            val plotData: Option[PlotData] =
              props.asterismTracking.map: tracking =>
                PlotData:
                  Map(
                    ObjectPlotData.Id(props.obsId.asLeft) ->
                      ObjectPlotData(
                        NonEmptyString.from(props.obsId.toString).getOrElse("Observation".refined),
                        tracking,
                        obsConf.configuration.foldMap(conf => List(conf.siteFor))
                      )
                  )

            val skyPlotTile: Option[Tile[?]] =
              plotData.map:
                ElevationPlotTile(
                  props.vault.userId,
                  ObsTabTileIds.PlotId.id,
                  _,
                  props.observation.get.observingMode.map(_.siteFor),
                  obsTimeView.get,
                  obsDuration.map(_.toDuration),
                  schedulingWindows.get,
                  props.globalPreferences.get,
                  "No target selected"
                )

            def getObsInfo(obsId: Observation.Id)(targetId: Target.Id): TargetEditObsInfo =
              TargetEditObsInfo.fromProgramSummaries(
                targetId,
                ObsIdSet.one(obsId).some,
                props.programSummaries
              )

            def setCurrentTarget(
              tid: Option[Target.Id],
              via: SetRouteVia
            ): Callback =
              // Set the route base on the selected target
              ctx.setPageVia(
                (AppTab.Observations,
                 props.programId,
                 Focused(ObsIdSet.one(props.obsId).some, tid)
                ).some,
                via
              )

            def onCloneTarget(params: OnCloneParameters): Callback =
              setCurrentTarget(params.idToAdd.some, SetRouteVia.HistoryReplace)

            def onAsterismUpdate(params: OnAsterismUpdateParams): Callback =
              val targetForPage: Option[Target.Id] =
                if (params.areAddingTarget) params.targetId.some else none
              setCurrentTarget(targetForPage, SetRouteVia.HistoryReplace)

            val targetTile =
              AsterismEditorTile(
                props.vault.userId,
                ObsTabTileIds.TargetId.id,
                props.programId,
                ObsIdSet.one(props.obsId),
                props.obsAndTargets,
                basicConfiguration,
                obsTimeView,
                obsDurationView,
                obsConf,
                pendingTime,
                props.focusedTarget,
                setCurrentTarget,
                onCloneTarget,
                onAsterismUpdate,
                getObsInfo(props.obsId),
                props.searching,
                "Targets",
                props.globalPreferences,
                guideStarSelection,
                props.isDisabled,
                // Any target changes invalidate the sequence
                sequenceChanged.set(pending)
              )

            val constraintsSelector: VdomNode =
              makeConstraintsSelector(
                props.obsId,
                props.observation.model.zoom(Observation.constraints),
                props.allConstraintSets,
                props.isDisabled
              )

            // The ExploreStyles.ConstraintsTile css adds a z-index to the constraints tile react-grid wrapper
            // so that the constraints selector dropdown always appears in front of any other tiles. If more
            // than one tile ends up having dropdowns in the tile header, we'll need something more complex such
            // as changing the css classes on the various tiles when the dropdown is clicked to control z-index.
            val constraintsTile =
              Tile(
                ObsTabTileIds.ConstraintsId.id,
                "Constraints"
              )(
                renderInTitle =>
                  ConstraintsPanel(
                    ObsIdSet.one(props.obsId),
                    props.observation.zoom(Observation.constraints),
                    props.isDisabled
                  ),
                (_, _) => constraintsSelector
              )

            val schedulingWindowsTile =
              SchedulingWindowsTile(schedulingWindows, props.isDisabled, false)

            val configurationTile: Tile[?] =
              ConfigurationTile(
                props.vault.userId,
                props.programId,
                props.obsId,
                props.observation.zoom(Observation.scienceRequirements),
                props.observation
                  .zoom((Observation.posAngleConstraint, Observation.observingMode).disjointZip),
                props.observation.get.scienceTargetIds,
                targetCoords,
                obsConf,
                selectedConfig,
                props.observation.get.toInstrumentConfig(props.obsTargets),
                props.modes,
                props.obsTargets,
                props.programSummaries.observingModeGroups,
                sequenceChanged.mod:
                  case Ready(x) => pending
                  case x        => x
                ,
                props.isDisabled,
                props.globalPreferences.get.wavelengthUnits
              )

            val alltiles =
              List(
                notesTile.some,
                targetTile.some,
                if (!props.vault.isGuest) finderChartsTile.some else none,
                skyPlotTile,
                constraintsTile.some,
                schedulingWindowsTile.some,
                configurationTile.some,
                sequenceTile.some,
                itcTile.some
              ).flattenOption

            val removedIds = ExploreGridLayouts.observations.removedTiles(props.calibrationRole)

            val tiles =
              alltiles.filterNot(t => removedIds.contains(t.id))

            TileController(
              props.vault.userId,
              props.resize.width.getOrElse(0),
              defaultLayout,
              layout,
              tiles,
              section,
              props.backButton.some
            )
