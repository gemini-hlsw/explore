// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.ErrorPolicy
import clue.FetchClient
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import explore.*
import explore.common.TimingWindowsQueries
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.constraints.ConstraintsPanel
import explore.itc.ItcProps
import explore.model.LoadingState
import explore.model.ObsSummary.observingMode
import explore.model.*
import explore.model.display.given
import explore.model.enums.AgsState
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.extensions.*
import explore.model.itc.ItcChartResult
import explore.model.itc.ItcTarget
import explore.model.layout.*
import explore.observationtree.obsEditAttachments
import explore.syntax.ui.*
import explore.timingwindows.TimingWindowsPanel
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.skycalc.averageParallacticAngle
import lucuma.core.model.ConstraintSet
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.User
import lucuma.core.model.{ObsAttachment => ObsAtt}
import lucuma.core.syntax.all.*
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Dropdown
import lucuma.react.primereact.SelectItem
import lucuma.react.resizeDetector.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.TargetWithId
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.common.ObsQueriesGQL.*
import queries.schemas.odb.ObsQueries
import queries.schemas.odb.ObsQueries.*

import java.time.Instant
import scala.collection.immutable.SortedSet
import lucuma.core.enums.ObsStatus

case class ObsTabTiles(
  vault:                    Option[UserVault],
  userId:                   Option[User.Id],
  programId:                Program.Id,
  backButton:               VdomNode,
  observation:              UndoSetter[ObsSummary],
  allTargets:               UndoSetter[TargetList],
  allConstraintSets:        Set[ConstraintSet],
  targetObservations:       Map[Target.Id, SortedSet[Observation.Id]],
  focusedTarget:            Option[Target.Id],
  searching:                View[Set[Target.Id]],
  defaultLayouts:           LayoutsMap,
  layouts:                  LayoutsMap,
  resize:                   UseResizeDetectorReturn,
  obsAttachments:           View[ObsAttachmentList],
  obsAttachmentAssignments: ObsAttachmentAssignmentMap,
  globalPreferences:        View[GlobalPreferences]
) extends ReactFnProps(ObsTabTiles.component):
  val obsId: Observation.Id = observation.get.id

object ObsTabTiles:
  private type Props = ObsTabTiles

  private def makeConstraintsSelector(
    programId:         Program.Id,
    observationId:     Observation.Id,
    constraintSet:     View[ConstraintSet],
    allConstraintSets: Set[ConstraintSet]
  )(using FetchClient[IO, ObservationDB]): VdomNode =
    <.div(
      ExploreStyles.JustifiedEndTileControl,
      Dropdown[ConstraintSet](
        value = constraintSet.get,
        onChange = (cs: ConstraintSet) =>
          constraintSet.set(cs) >>
            ObsQueries
              .updateObservationConstraintSet[IO](programId, List(observationId), cs)
              .runAsyncAndForget,
        options = allConstraintSets
          .map(cs => new SelectItem[ConstraintSet](value = cs, label = cs.shortName))
          .toList
      )
    )

  private def itcQueryProps(
    obs:            ObsSummary,
    odbItc:         Option[OdbItcResult.Success],
    selectedConfig: Option[BasicConfigAndItc],
    targetsList:    TargetList
  ): ItcProps =
    ItcProps(
      obs,
      odbItc.map(_.toItcExposureTime),
      selectedConfig,
      targetsList,
      obs.toModeOverride
    )

  private case class Offsets(
    science:     Option[NonEmptyList[Offset]],
    acquisition: Option[NonEmptyList[Offset]]
  )

  given Reusability[LoadingState] = Reusability.byEq

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStreamResourceOnMountBy { (props, ctx) =>
        import ctx.given

        ObsItcQuery[IO]
          .query(props.obsId)(ErrorPolicy.RaiseOnNoData)
          .map(
            _.data.observation.flatMap(
              _.itc.map(i =>
                OdbItcResult.Success(
                  i.result.exposureTime,
                  i.result.exposures,
                  i.result.signalToNoise
                )
              )
            )
          )
          // TODO Could we get the edit signal from ProgramCache instead of doing another subscritpion??
          .reRunOnResourceSignals(ObservationEditSubscription.subscribe[IO](props.obsId))
      }
      .useStreamResourceOnMountBy { (props, ctx, _) =>
        import ctx.given

        SequenceOffsets[IO]
          .query(props.obsId)
          .map(data =>
            Offsets(
              science = NonEmptyList.fromList(
                data.observation
                  .foldMap(_.execution.config.allScienceOffsets)
                  .distinct
              ),
              acquisition = NonEmptyList.fromList(
                data.observation.foldMap(_.execution.config.allAcquisitionOffsets).distinct
              )
            )
          )
          // TODO Could we get the edit signal from ProgramCache instead of doing another subscritpion??
          .reRunOnResourceSignals(ObservationEditSubscription.subscribe[IO](props.obsId))
      }
      // Ags state
      .useStateView[AgsState](AgsState.Idle)
      // Selected GS. to share the PA chosen for Unconstrained and average modes
      // This should go to the db eventually
      .useStateView(none[AgsAnalysis])
      // the configuration the user has selected from the spectroscopy modes table, if any
      .useStateView(none[BasicConfigAndItc])
      .useStateWithReuseBy((props, _, odbItc, _, _, _, selectedConfig) =>
        itcQueryProps(
          props.observation.get,
          odbItc.toOption.flatten,
          selectedConfig.get,
          props.allTargets.get
        )
      )
      // Chart results
      .useState(Map.empty[ItcTarget, Pot[ItcChartResult]])
      // itc loading
      .useStateWithReuse(LoadingState.Done)
      .useEffectWithDepsBy { (props, _, odbItc, _, _, _, selectedConfig, _, _, _) =>
        itcQueryProps(
          props.observation.get,
          odbItc.toOption.flatten,
          selectedConfig.get,
          props.allTargets.get
        )
      } { (props, ctx, _, _, _, _, _, oldItcProps, charts, loading) => itcProps =>
        import ctx.given

        oldItcProps.setState(itcProps).when_(itcProps.isExecutable) *>
          itcProps
            .requestITCData(
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
      .useEffectWithDepsBy((_, _, _, _, _, _, _, itcProps, _, _, _) => itcProps.value)(
        (_, _, _, _, _, _, _, _, _, _, selectedTarget) =>
          itcProps => selectedTarget.set(itcProps.defaultSelectedTarget)
      )
      // selected attachment
      .useStateView(none[ObsAtt.Id])
      .render {
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
          selectedAttachment
        ) =>
          import ctx.given

          // This view is shared between AGS and the configuration editor
          // when PA changes it gets saved to the db
          val posAngleConstraintView: View[PosAngleConstraint] =
            props.observation.model
              .zoom(ObsSummary.posAngleConstraint)
              .withOnMod(pa =>
                ObsQueries
                  .updatePosAngle[IO](props.programId, List(props.obsId), pa)
                  .switching(agsState.async, AgsState.Saving, AgsState.Idle)
                  .runAsync
              )

          val asterismIds: View[AsterismIds] =
            props.observation.model.zoom(ObsSummary.scienceTargetIds)

          val basicConfiguration: Option[BasicConfiguration] =
            props.observation.get.observingMode.map(_.toBasicConfiguration)

          val vizTimeView: View[Option[Instant]] =
            props.observation.model.zoom(ObsSummary.visualizationTime)

          val vizTime: Option[Instant] = vizTimeView.get

          val asterismAsNel: Option[NonEmptyList[TargetWithId]] =
            NonEmptyList.fromList(
              props.observation.get.scienceTargetIds.toList
                .map(id => props.allTargets.get.get(id).map(t => TargetWithId(id, t)))
                .flattenOption
            )

          // asterism base coordinates at viz time or default to base coordinates
          val targetCoords: Option[CoordinatesAtVizTime] =
            (vizTime, asterismAsNel)
              .mapN((instant, asterismNel) =>
                asterismNel.baseTrackingAt(instant).flatMap(_.at(instant))
              )
              .flatten
              // If e.g. vizTime isn't defined default to the asterism base coordinates
              .orElse(
                asterismAsNel
                  .map(asterismNel =>
                    CoordinatesAtVizTime(asterismNel.baseTracking.baseCoordinates)
                  )
              )

          val attachmentsView =
            props.observation.model.zoom(ObsSummary.attachmentIds).withOnMod { ids =>
              obsEditAttachments(props.programId, props.obsId, ids).runAsync
            }

          val finderChartsTile =
            FinderChartsTile.finderChartsTile(
              props.programId,
              props.obsId,
              attachmentsView,
              props.vault.map(_.token),
              props.obsAttachments,
              selectedAttachment
            )

          val showSequenceTile = props.observation.get.status === ObsStatus.Ready

          val notesTile =
            Tile(
              ObsTabTilesIds.NotesId.id,
              s"Note for Observer",
              canMinimize = true,
              hidden = !showSequenceTile
            )(_ =>
              <.div(
                ExploreStyles.NotesWrapper,
                <.div(
                  ExploreStyles.ObserverNotes,
                  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus maximus hendrerit lacinia. Etiam dapibus blandit ipsum sed rhoncus."
                )
              )
            )

          val itcTile: Tile =
            ItcTile.itcTile(
              props.userId,
              props.obsId,
              selectedItcTarget,
              props.allTargets.get,
              itcProps.value,
              itcChartResults.value,
              itcLoading.value,
              props.globalPreferences
            )

          val constraints: View[ConstraintSet] =
            props.observation.model.zoom(ObsSummary.constraints)

          val constraintsSelector: VdomNode =
            makeConstraintsSelector(
              props.programId,
              props.obsId,
              props.observation.model.zoom(ObsSummary.constraints),
              props.allConstraintSets
            )

          val timingWindows: View[List[TimingWindow]] =
            TimingWindowsQueries.viewWithRemoteMod(
              props.programId,
              ObsIdSet.one(props.obsId),
              props.observation.undoableView[List[TimingWindow]](ObsSummary.timingWindows)
            )

          val skyPlotTile: Tile =
            ElevationPlotTile.elevationPlotTile(
              props.userId,
              props.focusedTarget.orElse(props.observation.get.scienceTargetIds.headOption),
              props.observation.get.observingMode.map(_.siteFor),
              targetCoords,
              vizTime,
              timingWindows.get,
              props.globalPreferences.get
            )

          def setCurrentTarget(programId: Program.Id, oid: Option[Observation.Id])(
            tid: Option[Target.Id],
            via: SetRouteVia
          ): Callback =
            // Set the route base on the selected target
            ctx.setPageVia(
              AppTab.Observations,
              programId,
              Focused(oid.map(ObsIdSet.one), tid),
              via
            )

          val paProps: PAProperties =
            PAProperties(props.obsId, selectedPA, agsState, posAngleConstraintView)

          val averagePA: Option[Angle] =
            (basicConfiguration.map(_.siteFor), asterismAsNel, vizTime)
              .mapN((site, asterism, vizTime) =>
                posAngleConstraintView.get match
                  case PosAngleConstraint.AverageParallactic =>
                    averageParallacticAngle(site, asterism.baseTracking, vizTime)
                  case _                                     => none
              )
              .flatten

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
              averagePA
            )

          def otherObsCount(obsId: Observation.Id, targetId: Target.Id): Int =
            props.targetObservations.get(targetId).fold(0)(obsIds => (obsIds - obsId).size)

          val targetTile: Tile =
            AsterismEditorTile.asterismEditorTile(
              props.userId,
              props.programId,
              ObsIdSet.one(props.obsId),
              asterismIds,
              props.allTargets,
              basicConfiguration,
              vizTimeView,
              obsConf,
              props.focusedTarget,
              setCurrentTarget(props.programId, props.obsId.some),
              otherObsCount(props.obsId, _),
              props.searching,
              "Targets",
              props.globalPreferences
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
              control = _ => constraintsSelector.some,
              controllerClass = ExploreStyles.ConstraintsTile
            )(renderInTitle =>
              <.div
              ConstraintsPanel(
                props.programId,
                ObsIdSet.one(props.obsId),
                props.observation.zoom(ObsSummary.constraints),
                renderInTitle
              )
            )

          val timingWindowsTile =
            Tile(ObsTabTilesIds.TimingWindowsId.id, "Scheduling Windows", canMinimize = true)(
              renderInTitle => TimingWindowsPanel(timingWindows, renderInTitle)
            )

          val configurationTile =
            ConfigurationTile.configurationTile(
              props.userId,
              props.programId,
              props.obsId,
              props.observation.get.title,
              props.observation.get.subtitle,
              props.observation.zoom(ObsSummary.scienceRequirements),
              props.observation.zoom(ObsSummary.observingMode),
              posAngleConstraintView,
              props.observation.get.scienceTargetIds,
              targetCoords,
              obsConf,
              selectedConfig,
              props.allTargets.get
            )

          TileController(
            props.userId,
            props.resize.width.getOrElse(0),
            props.defaultLayouts,
            props.layouts,
            List(
              notesTile,
              targetTile,
              finderChartsTile,
              skyPlotTile,
              constraintsTile,
              timingWindowsTile,
              configurationTile,
              itcTile
            ),
            GridLayoutSection.ObservationsLayout,
            props.backButton.some,
            clazz = ExploreStyles.ObservationTiles.some
          )
      }
