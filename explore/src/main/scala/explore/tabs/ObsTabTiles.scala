// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.ErrorPolicy
import clue.FetchClient
import crystal.*
import crystal.Pot.Ready
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.common.TimingWindowsQueries
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.config.sequence.SequenceEditorTile
import explore.constraints.ConstraintsPanel
import explore.findercharts.ChartSelector
import explore.itc.ItcProps
import explore.model.*
import explore.model.AppContext
import explore.model.LoadingState
import explore.model.Observation
import explore.model.Observation.observingMode
import explore.model.ObservationsAndTargets
import explore.model.OnCloneParameters
import explore.model.ProgramSummaries
import explore.model.TargetEditObsInfo
import explore.model.display.given
import explore.model.enums.AgsState
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.extensions.*
import explore.model.itc.ItcChartResult
import explore.model.itc.ItcExposureTime
import explore.model.itc.ItcTarget
import explore.model.layout.*
import explore.modes.SpectroscopyModesMatrix
import explore.observationtree.obsEditAttachments
import explore.syntax.ui.*
import explore.timingwindows.TimingWindowsPanel
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.skycalc.averageParallacticAngle
import lucuma.core.model.ConstraintSet
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.ObsAttachment as ObsAtt
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.syntax.all.*
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Dropdown
import lucuma.react.primereact.SelectItem
import lucuma.react.resizeDetector.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.odb.input.*
import lucuma.ui.reusability.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.common.ObsQueriesGQL.*
import queries.schemas.odb.ObsQueries
import queries.schemas.odb.ObsQueries.*

import java.time.Instant
import scala.collection.immutable.SortedSet

case class ObsTabTiles(
  vault:             Option[UserVault],
  programId:         Program.Id,
  modes:             SpectroscopyModesMatrix,
  backButton:        VdomNode,
  observation:       UndoSetter[Observation],
  obsAndTargets:     UndoSetter[ObservationsAndTargets],
  obsAttachments:    View[ObsAttachmentList],
  programSummaries:  ProgramSummaries,
  focusedTarget:     Option[Target.Id],
  searching:         View[Set[Target.Id]],
  defaultLayouts:    LayoutsMap,
  layouts:           LayoutsMap,
  resize:            UseResizeDetectorReturn,
  globalPreferences: View[GlobalPreferences],
  readonly:          Boolean
) extends ReactFnProps(ObsTabTiles.component):
  val obsId: Observation.Id                                         = observation.get.id
  val isDisabled: Boolean                                           = readonly || observation.get.isCalibration
  val allConstraintSets: Set[ConstraintSet]                         = programSummaries.constraintGroups.map(_._2).toSet
  val targetObservations: Map[Target.Id, SortedSet[Observation.Id]] =
    programSummaries.targetObservations
  val obsExecution: Pot[Execution]                                  = programSummaries.obsExecutionPots.getPot(obsId)
  val allTargets: TargetList                                        = programSummaries.targets
  val obsAttachmentAssignments: ObsAttachmentAssignmentMap          =
    programSummaries.obsAttachmentAssignments

object ObsTabTiles:
  private type Props = ObsTabTiles

  private def makeConstraintsSelector(
    observationId:     Observation.Id,
    constraintSet:     View[ConstraintSet],
    allConstraintSets: Set[ConstraintSet],
    isDisabled:        Boolean
  )(using FetchClient[IO, ObservationDB]): VdomNode =
    <.div(
      ExploreStyles.JustifiedEndTileControl,
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

  private def itcQueryProps(
    obs:             Observation,
    itcExposureTime: Option[ItcExposureTime],
    selectedConfig:  Option[BasicConfigAndItc],
    targetsList:     TargetList
  ): ItcProps =
    ItcProps(
      obs,
      itcExposureTime,
      selectedConfig,
      targetsList,
      obs.toModeOverride
    )

  private case class Offsets(
    science:     Option[NonEmptyList[Offset]],
    acquisition: Option[NonEmptyList[Offset]]
  )

  private def expTime(
    selectedConfig: Option[BasicConfigAndItc],
    odbItc:         Option[OdbItcResult.Success]
  ): Option[ItcExposureTime] =
    val tableItc = for {
      conf <- selectedConfig
      res  <- conf.itcResult.flatMap(_.toOption)
      itc  <- res.toItcExposureTime
    } yield itc
    odbItc.map(_.toItcExposureTime).orElse(tableItc)

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStreamResourceOnMountBy: (props, ctx) =>
        import ctx.given

        ObsItcQuery[IO]
          .query(props.obsId)(ErrorPolicy.RaiseOnNoData)
          .map:
            _.data.observation.map: o =>
              OdbItcResult.Success(
                o.itc.science.selected.exposureTime,
                o.itc.science.selected.exposures,
                o.itc.science.selected.signalToNoise,
                o.itc.acquisition.selected.exposures,
                o.itc.acquisition.selected.signalToNoise
              )
          // TODO Could we get the edit signal from ProgramCache instead of doing another subscritpion??
          .reRunOnResourceSignals:
            ObservationEditSubscription.subscribe[IO](props.obsId.toObservationEditInput)
      .useStreamResourceOnMountBy: (props, ctx, _) =>
        import ctx.given

        SequenceOffsets[IO]
          .query(props.obsId)
          .map: data =>
            Offsets(
              science = NonEmptyList.fromList(
                data.observation
                  .foldMap(_.execution.config.toList.flatMap(_.allScienceOffsets))
                  .distinct
              ),
              acquisition = NonEmptyList.fromList(
                data.observation
                  .foldMap(_.execution.config.toList.flatMap(_.allAcquisitionOffsets))
                  .distinct
              )
            )
          // TODO Could we get the edit signal from ProgramCache instead of doing another subscritpion??
          .reRunOnResourceSignals:
            ObservationEditSubscription.subscribe[IO](props.obsId.toObservationEditInput)
      // Ags state
      .useStateView[AgsState](AgsState.Idle)
      // Selected GS. to share the PA chosen for Unconstrained and average modes
      // This should go to the db eventually
      .useStateView(none[AgsAnalysis])
      // the configuration the user has selected from the spectroscopy modes table, if any
      .useStateView(none[BasicConfigAndItc])
      .useStateWithReuseBy: (props, _, odbItc, _, _, _, selectedConfig) =>
        val time = expTime(selectedConfig.get, odbItc.toOption.flatten)

        itcQueryProps(
          props.observation.get,
          time,
          selectedConfig.get,
          props.allTargets
        )
      // Chart results
      .useState(Map.empty[ItcTarget, Pot[ItcChartResult]])
      // itc loading
      .useStateWithReuse(LoadingState.Done)
      .useEffectWithDepsBy { (props, _, odbItc, _, _, _, selectedConfig, _, _, _) =>
        val time = expTime(selectedConfig.get, odbItc.toOption.flatten)

        itcQueryProps(
          props.observation.get,
          time,
          selectedConfig.get,
          props.allTargets
        )
      } { (props, ctx, _, _, _, _, _, oldItcProps, charts, loading) => itcProps =>
        import ctx.given

        oldItcProps.setState(itcProps).when_(itcProps.isExecutable) *>
          itcProps
            .requestChart(
              m => {
                val r = m.map {
                  case (k, Left(e))  =>
                    k -> (Pot.error(new RuntimeException(e.shortName)): Pot[ItcChartResult])
                  case (k, Right(e)) =>
                    k -> (Pot.Ready(e): Pot[ItcChartResult])
                }.toMap
                charts
                  .setStateAsync(r) *> loading.setState(LoadingState.Done).value.toAsync
              },
              (charts.setState(
                itcProps.targets
                  .map(t =>
                    t -> Pot.error(
                      new RuntimeException("Not enough information to calculate the ITC graph")
                    )
                  )
                  .toMap
              ) *> loading.setState(LoadingState.Done)).toAsync,
              loading.setState(LoadingState.Loading).value.toAsync
            )
            .whenA(itcProps.isExecutable)
            .runAsyncAndForget
      }
      // ITC selected target. Here to be shared by the ITC tile body and title
      .useStateView(none[ItcTarget])
      // Reset the selected target if itcProps changes
      .useEffectWithDepsBy((_, _, _, _, _, _, _, itcProps, _, _, _) => itcProps.value):
        (_, _, _, _, _, _, _, _, _, _, selectedTarget) =>
          itcProps => selectedTarget.set(itcProps.defaultSelectedTarget)
      // selected attachment
      .useStateView(none[ObsAtt.Id])
      // Signal that the sequence has changed
      .useStateView(().ready)
      .useStateView(ChartSelector.Closed)
      .useEffectKeepResultWithDepsBy((p, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        p.observation.model.get.visualizationTime
      ): (_, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        vizTime => IO(vizTime.getOrElse(Instant.now()))
      .render:
        (
          props,
          ctx,
          itc,
          sequenceOffsets,
          agsState,
          selectedPA,
          selectedConfig,
          itcProps,
          itcChartResults,
          itcLoading,
          selectedItcTarget,
          selectedAttachment,
          sequenceChanged,
          chartSelector,
          vizTimeOrNowPot
        ) =>
          import ctx.given

          vizTimeOrNowPot.renderPot: vizTimeOrNow =>
            // This view is shared between AGS and the configuration editor
            // when PA changes it gets saved to the db
            val posAngleConstraintView: View[PosAngleConstraint] =
              props.observation.model
                .zoom(Observation.posAngleConstraint)
                .withOnMod(pa =>
                  ObsQueries
                    .updatePosAngle[IO](List(props.obsId), pa)
                    .switching(agsState.async, AgsState.Saving, AgsState.Idle)
                    .runAsync
                )

            val asterismIds: View[AsterismIds] =
              props.observation.model.zoom(Observation.scienceTargetIds)

            val basicConfiguration: Option[BasicConfiguration] =
              props.observation.get.observingMode.map(_.toBasicConfiguration)

            val vizTimeView: View[Option[Instant]] =
              props.observation.model.zoom(Observation.visualizationTime)

            val asterismAsNel: Option[NonEmptyList[TargetWithId]] =
              NonEmptyList.fromList:
                props.observation.get.scienceTargetIds.toList
                  .map(id => props.allTargets.get(id).map(t => TargetWithId(id, t)))
                  .flattenOption

            // asterism base coordinates at viz time or current time
            val targetCoords: Option[CoordinatesAtVizTime] =
              asterismAsNel
                .flatMap(asterismNel => asterismNel.baseTracking.at(vizTimeOrNow))

            val attachmentsView =
              props.observation.model.zoom(Observation.attachmentIds).withOnMod { ids =>
                obsEditAttachments(props.obsId, ids).runAsync
              }

            val pendingTime = props.obsExecution.toOption.flatMap(_.programTimeEstimate)

            val paProps: PAProperties =
              PAProperties(props.obsId, selectedPA, agsState, posAngleConstraintView)

            val averagePA: Option[AveragePABasis] =
              (basicConfiguration.map(_.siteFor), asterismAsNel, pendingTime)
                .mapN: (site, asterism, pendingTime) =>
                  posAngleConstraintView.get match
                    case PosAngleConstraint.AverageParallactic =>
                      // See also `anglesToTestAt` in AladinCell.scala.
                      averageParallacticAngle(
                        site,
                        asterism.baseTracking,
                        vizTimeOrNow,
                        pendingTime.toDuration
                      ).map(AveragePABasis(vizTimeOrNow, pendingTime, _))
                    case _                                     => none
                .flatten

            // The angle used for `Align to PA` in the finder charts tile.
            // For Unbounded, use the PA of the currently selected guide star (if any)
            // For AverageParllactic constraint, use the average PA (if any), otherwise
            // use the angle specified in the constraint
            val pa: Option[Angle] =
              posAngleConstraintView.get match
                case PosAngleConstraint.Unbounded                  => paProps.selectedPA
                case PosAngleConstraint.AverageParallactic         => averagePA.map(_.averagePA)
                case PosAngleConstraint.Fixed(angle)               => angle.some
                case PosAngleConstraint.AllowFlip(angle)           => angle.some
                case PosAngleConstraint.ParallacticOverride(angle) => angle.some

            val finderChartsTile =
              FinderChartsTile.finderChartsTile(
                props.programId,
                props.obsId,
                attachmentsView,
                props.vault.map(_.token),
                props.obsAttachments,
                selectedAttachment,
                pa,
                chartSelector,
                props.isDisabled
              )

            val notesView: View[Option[NonEmptyString]] =
              props.observation.model
                .zoom(Observation.observerNotes)
                .withOnMod: notes =>
                  ObsQueries
                    .updateNotes[IO](List(props.obsId), notes)
                    .runAsync

            val notesTile = NotesTile.notesTile(props.obsId, notesView)

            val sequenceTile =
              SequenceEditorTile.sequenceTile(
                props.programId,
                props.obsId,
                props.obsExecution,
                asterismIds.get,
                itc.toOption.flatten,
                sequenceChanged
              )

            val itcTile: Tile =
              ItcTile.itcTile(
                props.vault.userId,
                props.obsId,
                selectedItcTarget,
                props.allTargets,
                itcProps.value,
                itcChartResults.value,
                itcLoading.value,
                props.globalPreferences
              )

            val constraints: View[ConstraintSet] =
              props.observation.model.zoom(Observation.constraints)

            val constraintsSelector: VdomNode =
              makeConstraintsSelector(
                props.obsId,
                props.observation.model.zoom(Observation.constraints),
                props.allConstraintSets,
                props.isDisabled
              )

            val timingWindows: View[List[TimingWindow]] =
              TimingWindowsQueries.viewWithRemoteMod(
                ObsIdSet.one(props.obsId),
                props.observation.undoableView[List[TimingWindow]](Observation.timingWindows)
              )

            val skyPlotTile: Tile =
              ElevationPlotTile.elevationPlotTile(
                props.vault.userId,
                props.focusedTarget.orElse(props.observation.get.scienceTargetIds.headOption),
                props.observation.get.observingMode.map(_.siteFor),
                targetCoords,
                vizTimeView.get,
                pendingTime.map(_.toDuration),
                timingWindows.get,
                props.globalPreferences.get
              )

            val obsConf =
              ObsConfiguration(
                basicConfiguration,
                paProps.some,
                constraints.get.some,
                ScienceRequirements.spectroscopy
                  .getOption(props.observation.get.scienceRequirements)
                  .flatMap(_.wavelength),
                sequenceOffsets.toOption.flatMap(_.science),
                sequenceOffsets.toOption.flatMap(_.acquisition),
                averagePA,
                pendingTime.map(_.toDuration)
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
                AppTab.Observations,
                props.programId,
                Focused(ObsIdSet.one(props.obsId).some, tid),
                via
              )

            def onCloneTarget(params: OnCloneParameters): Callback =
              setCurrentTarget(params.idToAdd.some, SetRouteVia.HistoryReplace)

            val targetTile: Tile =
              AsterismEditorTile.asterismEditorTile(
                props.vault.userId,
                props.programId,
                ObsIdSet.one(props.obsId),
                asterismIds,
                props.obsAndTargets,
                basicConfiguration,
                vizTimeView,
                obsConf,
                props.focusedTarget,
                setCurrentTarget,
                onCloneTarget,
                getObsInfo(props.obsId),
                props.searching,
                "Targets",
                props.globalPreferences,
                props.isDisabled,
                // Any target changes invalidate the sequence
                sequenceChanged.set(Pot.pending)
              )

            // The ExploreStyles.ConstraintsTile css adds a z-index to the constraints tile react-grid wrapper
            // so that the constraints selector dropdown always appears in front of any other tiles. If more
            // than one tile ends up having dropdowns in the tile header, we'll need something more complex such
            // as changing the css classes on the various tiles when the dropdown is clicked to control z-index.
            val constraintsTile =
              Tile(
                ObsTabTilesIds.ConstraintsId.id,
                "Constraints",
                canMinimize = true,
                control = _ => constraintsSelector.some
              )(renderInTitle =>
                <.div
                ConstraintsPanel(
                  ObsIdSet.one(props.obsId),
                  props.observation.zoom(Observation.constraints),
                  props.isDisabled
                )
              )

            val timingWindowsTile =
              TimingWindowsPanel.timingWindowsPanel(timingWindows, props.isDisabled)

            val configurationTile =
              ConfigurationTile.configurationTile(
                props.vault.userId,
                props.programId,
                props.obsId,
                props.observation.zoom(Observation.scienceRequirements),
                props.observation.zoom(Observation.observingMode),
                posAngleConstraintView,
                props.observation.get.scienceTargetIds,
                targetCoords,
                obsConf,
                selectedConfig,
                props.modes,
                props.allTargets,
                sequenceChanged.mod {
                  case Ready(x) => Pot.pending
                  case x        => x
                },
                props.isDisabled
              )

            TileController(
              props.vault.userId,
              props.resize.width.getOrElse(0),
              props.defaultLayouts,
              props.layouts,
              List(
                notesTile.some,
                targetTile.some,
                if (!props.vault.isGuest) finderChartsTile.some else none,
                skyPlotTile.some,
                constraintsTile.some,
                timingWindowsTile.some,
                configurationTile.some,
                sequenceTile.some,
                itcTile.some
              ).flattenOption,
              GridLayoutSection.ObservationsLayout,
              props.backButton.some
            )
