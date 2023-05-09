// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.Order.given
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.ErrorPolicy
import clue.FetchClient
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.implicits.*
import explore.*
import explore.common.TimingWindowsQueries
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.constraints.ConstraintsPanel
import explore.model.AppContext
import explore.model.Asterism
import explore.model.AsterismIds
import explore.model.BasicConfigAndItc
import explore.model.ConstraintGroup
import explore.model.Focused
import explore.model.ModelUndoStacks
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ObsSummary
import explore.model.PAProperties
import explore.model.ProgramSummaries
import explore.model.TargetList
import explore.model.TargetWithObs
import explore.model.display.given
import explore.model.enums.AgsState
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.extensions.*
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcTarget
import explore.model.itc.OverridenExposureTime
import explore.model.layout.*
import explore.optics.*
import explore.optics.all.*
import explore.timingwindows.TimingWindowsPanel
import explore.undo.UndoContext
import explore.undo.UndoSetter
import explore.undo.UndoStacks
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.skycalc.averageParallacticAngle
import lucuma.core.model.ConstraintSet
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.User
import lucuma.core.syntax.all.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.TargetWithId
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Iso
import queries.common.ObsQueriesGQL.*
import queries.schemas.odb.ObsQueries
import queries.schemas.odb.ObsQueries.*
import react.common.ReactFnProps
import react.primereact.Dropdown
import react.primereact.SelectItem
import react.resizeDetector.*

import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

case class ObsTabTiles(
  userId:             Option[User.Id],
  programId:          Program.Id,
  backButton:         VdomNode,
  obsUndoCtx:         UndoSetter[ObsSummary],
  allTargetsUndoCtx:  UndoSetter[TargetList],
  allConstraintSets:  Set[ConstraintSet],
  targetObservations: Map[Target.Id, SortedSet[Observation.Id]],
  focusedTarget:      Option[Target.Id],
  undoStacks:         View[ModelUndoStacks[IO]],
  searching:          View[Set[Target.Id]],
  defaultLayouts:     LayoutsMap,
  layouts:            View[Pot[LayoutsMap]],
  resize:             UseResizeDetectorReturn
) extends ReactFnProps(ObsTabTiles.component):
  val observation: ObsSummary = obsUndoCtx.model.get
  val obsId: Observation.Id   = observation.id
  val allTargets: TargetList  = allTargetsUndoCtx.model.get

object ObsTabTiles:
  private type Props = ObsTabTiles

  private def makeConstraintsSelector(
    programId:         Program.Id,
    observationId:     Observation.Id,
    constraintSet:     View[ConstraintSet],
    allConstraintSets: Set[ConstraintSet]
  )(using FetchClient[IO, ObservationDB]): VdomNode =
    Dropdown[ConstraintSet](
      clazz = ExploreStyles.ConstraintsTileSelector,
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
  // }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStreamResourceViewOnMountBy { (props, ctx) =>
        import ctx.given

        ObsEditQuery[IO]
          .query(props.programId, props.obsId)(ErrorPolicy.RaiseOnNoData)
          .map(
            _.data.asObsEditData
              .getOrElse(throw new Exception(s"Observation [${props.obsId}] not found"))
          )
          .reRunOnResourceSignals(ObservationEditSubscription.subscribe[IO](props.obsId))
      }
      // ITC selected target. Here to be shared by the ITC tile body and title
      .useStateView(none[ItcTarget])
      // Ags state
      .useStateView[AgsState](AgsState.Idle)
      // Selected GS. to share the PA chosen for Unconstrained and average modes
      // This should go to the db eventually
      .useStateView(none[AgsAnalysis])
      // the configuration the user has selected from the spectroscopy modes table, if any
      .useStateView(none[BasicConfigAndItc])
      .render { (props, ctx, obsView, itcTarget, agsState, selectedPA, selectedConfig) =>
        import ctx.given

        val obsViewPot = obsView.toPot

        val observingMode: Option[ObservingMode] =
          obsView.toOption.flatMap(_.get.scienceData.mode)

        val posAngle: Option[View[PosAngleConstraint]] =
          obsView.toOption
            .map(
              _.zoom(ObsEditData.scienceData.andThen(ScienceData.posAngle))
            )

        val potAsterismIds: Pot[View[AsterismIds]] =
          obsViewPot.map(v =>
            v.zoom(
              ObsEditData.scienceData
                .andThen(ScienceData.targets)
                .andThen(ObservationData.TargetEnvironment.asterism)
                .andThen(ObsQueries.targetIdsFromAsterism)
            ).zoomSplitEpi(sortedSetFromList)
          )

        val basicConfiguration = observingMode.map(_.toBasicConfiguration)

        val vizTimeView: Pot[View[Option[Instant]]] =
          obsViewPot.map(_.zoom(ObsEditData.visualizationTime))

        val vizTime = vizTimeView.toOption.flatMap(_.get)

        val asterismAsNel: Option[NonEmptyList[TargetWithId]] =
          potAsterismIds.toOption.flatMap(asterismIdsView =>
            NonEmptyList.fromList(
              asterismIdsView.get.toList
                .map(id => props.allTargets.get(id).map(t => TargetWithId(id, t)))
                .flattenOption
            )
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
                .map(asterismNel => CoordinatesAtVizTime(asterismNel.baseTracking.baseCoordinates))
            )

        val spectroscopyReqs: Option[ScienceRequirementsData] =
          obsView.toOption.map(_.get.scienceData.requirements)

        val notesTile =
          Tile(
            ObsTabTilesIds.NotesId.id,
            s"Note for Observer",
            props.backButton.some,
            canMinimize = true
          )(_ =>
            <.div(
              ExploreStyles.NotesWrapper,
              <.div(
                ExploreStyles.ObserverNotes,
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus maximus hendrerit lacinia. Etiam dapibus blandit ipsum sed rhoncus."
              )
            )
          )

        val constraints =
          obsViewPot.map(_.zoom(ObsEditData.scienceData.andThen(ScienceData.constraints)))

        val scienceData = obsViewPot.toOption.map(a => ObsEditData.scienceData.get(a.get))

        val itcTile =
          ItcTile.itcTile(
            props.userId,
            props.obsId,
            observingMode,
            obsView.toOption.map(_.get.scienceData.requirements.spectroscopy),
            scienceData,
            obsView.toOption
              .flatMap(
                _.get.itcExposureTime
                  .map(r => ItcChartExposureTime(OverridenExposureTime.FromItc, r.time, r.count))
              ),
            itcTarget,
            selectedConfig.get,
            props.allTargets
          )

        val constraintsSelector =
          makeConstraintsSelector(
            props.programId,
            props.obsId,
            props.obsUndoCtx.model.zoom(ObsSummary.constraints),
            props.allConstraintSets
          )

        val skyPlotTile =
          ElevationPlotTile.elevationPlotTile(
            props.userId,
            props.focusedTarget.orElse(props.observation.scienceTargetIds.headOption),
            observingMode.map(_.siteFor),
            targetCoords,
            vizTime,
            props.observation.timingWindows
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

        val paProps = posAngle.map(p => PAProperties(props.obsId, selectedPA, agsState, p))

        val averagePA: Option[Angle] =
          (basicConfiguration.map(_.siteFor), asterismAsNel, vizTime)
            .mapN((site, asterism, vizTime) =>
              posAngle.map(_.get) match
                case Some(PosAngleConstraint.AverageParallactic) =>
                  averageParallacticAngle(site, asterism.baseTracking, vizTime)
                case _                                           => none
            )
            .flatten

        val obsConf = obsView.toOption.map(o =>
          ObsConfiguration(
            basicConfiguration,
            paProps,
            o.get.scienceData.constraints.some,
            o.get.scienceData.requirements.spectroscopy.wavelength,
            o.get.scienceData.scienceOffsets,
            o.get.scienceData.acquisitionOffsets,
            averagePA
          )
        )

        def otherObsCount(obsId: Observation.Id, targetId: Target.Id): Int =
          props.targetObservations.get(targetId).fold(0)(obsIds => (obsIds - obsId).size)

        val targetTile = AsterismEditorTile.asterismEditorTile(
          props.userId,
          props.programId,
          ObsIdSet.one(props.obsId),
          potAsterismIds,
          props.allTargetsUndoCtx.model,
          basicConfiguration,
          vizTimeView,
          obsConf,
          props.focusedTarget,
          setCurrentTarget(props.programId, props.obsId.some),
          otherObsCount(props.obsId, _),
          props.undoStacks.zoom(ModelUndoStacks.forSiderealTarget),
          props.searching,
          "Targets",
          backButton = none
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
            controllerClass = ExploreStyles.ConstraintsTile.some
          )(renderInTitle =>
            ConstraintsPanel(
              props.programId,
              ObsIdSet.one(props.obsId),
              props.obsUndoCtx.zoom(ObsSummary.constraints),
              renderInTitle
            )
          )

        val timingWindows: View[List[TimingWindow]] =
          TimingWindowsQueries.viewWithRemoteMod(
            props.programId,
            ObsIdSet.one(props.obsId),
            props.obsUndoCtx.undoableView[List[TimingWindow]](ObsSummary.timingWindows)
          )

        val timingWindowsTile =
          Tile(ObsTabTilesIds.TimingWindowsId.id, "Timing Windows", canMinimize = true)(
            renderInTitle => TimingWindowsPanel(timingWindows, renderInTitle)
          )

        val configurationTile =
          ConfigurationTile.configurationTile(
            props.userId,
            props.programId,
            props.obsId,
            obsViewPot.map(obsEditData =>
              (obsEditData.get.title,
               obsEditData.get.subtitle,
               obsEditData.zoom(ObsEditData.scienceData)
              )
            ),
            props.undoStacks
              .zoom(ModelUndoStacks.forObsScienceData[IO])
              .zoom(atMapWithDefault(props.obsId, UndoStacks.empty)),
            targetCoords,
            obsConf,
            selectedConfig,
            props.allTargets
          )

        props.layouts.renderPotView(l =>
          TileController(
            props.userId,
            props.resize.width.getOrElse(0),
            props.defaultLayouts,
            l,
            List(
              notesTile,
              targetTile,
              skyPlotTile,
              constraintsTile,
              timingWindowsTile,
              configurationTile,
              itcTile
            ),
            GridLayoutSection.ObservationsLayout,
            clazz = ExploreStyles.ObservationTiles.some
          )
        )
      }
