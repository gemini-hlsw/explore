// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.Async
import cats.effect.IO
import cats.implicits._
import clue.TransactionalClient
import clue.data.syntax._
import crystal.react.View
import crystal.react.reuse._
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.AirMassRange
import explore.model.ConstraintGroup
import explore.model.ConstraintSet
import explore.model.HourAngleRange
import explore.model.ObsIdSet
import explore.model.ObsSummaryWithTargetsAndConstraints
import explore.model.TargetSummary
import explore.model.reusability._
import explore.optics._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Getter
import monocle.Lens
import monocle.macros.GenIso

import scala.collection.immutable.SortedMap

import ObsQueriesGQL._

object ObsQueries {

  type ObservationList = KeyedIndexedList[Observation.Id, ObsSummaryWithTargetsAndConstraints]
  type ConstraintsList = SortedMap[ObsIdSet, ConstraintGroup]

  type WavelengthInput = ObservationDB.Types.WavelengthModelInput
  val WavelengthInput = ObservationDB.Types.WavelengthModelInput
  type ObservationData = ObsEditQuery.Data.Observation
  val ObservationData = ObsEditQuery.Data.Observation
  type ScienceRequirementsData = ObservationData.ScienceRequirements
  val ScienceRequirementsData = ObservationData.ScienceRequirements
  type Targets                      = ObservationData.Targets
  type SpectroscopyRequirementsData = ObservationData.ScienceRequirements.SpectroscopyRequirements
  val SpectroscopyRequirementsData = ObservationData.ScienceRequirements.SpectroscopyRequirements
  type ScienceConfigurationData = ObservationData.ScienceConfiguration
  val ScienceConfigurationData = ObservationData.ScienceConfiguration

  case class ScienceData(
    requirements:  ScienceRequirementsData,
    configuration: Option[ScienceConfigurationData],
    constraints:   ConstraintSet,
    targets:       Targets
  )

  object ScienceData {
    val requirements: Lens[ScienceData, ScienceRequirementsData]           =
      Focus[ScienceData](_.requirements)
    val configuration: Lens[ScienceData, Option[ScienceConfigurationData]] =
      Focus[ScienceData](_.configuration)
    val constraints: Lens[ScienceData, ConstraintSet]                      =
      Focus[ScienceData](_.constraints)
    implicit val reusabilityScienceData: Reusability[ScienceData]          = Reusability.derive
  }

  val scienceDataForObs: Lens[ObservationData, ScienceData] =
    disjointZip(ObservationData.scienceRequirements,
                ObservationData.scienceConfiguration,
                ObservationData.constraintSet,
                ObservationData.targets
    )
      .andThen(GenIso.fields[ScienceData].reverse)

  case class ObsSummariesWithConstraints(
    observations:     ObservationList,
    constraintGroups: ConstraintsList
  )

  object ObsSummariesWithConstraints {
    val observations     = Focus[ObsSummariesWithConstraints](_.observations)
    val constraintGroups = Focus[ObsSummariesWithConstraints](_.constraintGroups)

    implicit val reusabilityObsSummaryWithConstraints: Reusability[ObsSummariesWithConstraints] =
      Reusability.derive
  }

  private def convertTarget(
    target: ProgramObservationsQuery.Data.Observations.Nodes.Targets.Asterism
  ): TargetSummary =
    TargetSummary(target.id, target.name)

  private val queryToObsSummariesWithConstraintsGetter
    : Getter[ProgramObservationsQuery.Data, ObsSummariesWithConstraints] = data =>
    ObsSummariesWithConstraints(
      KeyedIndexedList.fromList(
        data.observations.nodes.map(node =>
          ObsSummaryWithTargetsAndConstraints(
            node.id,
            node.targets.asterism.map(convertTarget),
            node.constraintSet,
            node.status,
            node.activeStatus,
            node.plannedTime.execution
          )
        ),
        ObsSummaryWithTargetsAndConstraints.id.get
      ),
      data.constraintSetGroup.nodes.toSortedMap(ConstraintGroup.obsIds.get)
    )

  implicit class ProgramObservationsQueryDataOps(val self: ProgramObservationsQuery.Data.type)
      extends AnyVal {
    def asObsSummariesWithConstraints = queryToObsSummariesWithConstraintsGetter
  }

  val ObsLiveQuery =
    ScalaFnComponent[View[ObsSummariesWithConstraints] ==> VdomNode](render =>
      AppCtx.using { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB,
                           ProgramObservationsQuery.Data,
                           ObsSummariesWithConstraints
        ](
          ProgramObservationsQuery.query().reuseAlways,
          (ProgramObservationsQuery.Data.asObsSummariesWithConstraints.get _).reuseAlways,
          List(
            ProgramObservationsEditSubscription.subscribe[IO]()
          ).reuseAlways
        )(potRender(render))
      }
    )

  def updateObservationConstraintSet[F[_]: Async](
    obsIds:      List[Observation.Id],
    constraints: ConstraintSet
  )(implicit
    c:           TransactionalClient[F, ObservationDB]
  ): F[Unit] = {
    val createER: CreateElevationRangeInput = constraints.elevationRange match {
      case AirMassRange(min, max)   =>
        CreateElevationRangeInput(airmassRange =
          CreateAirmassRangeInput(min = min.value, max = max.value).assign
        )
      case HourAngleRange(min, max) =>
        CreateElevationRangeInput(hourAngleRange =
          CreateHourAngleRangeInput(minHours = min.value, maxHours = max.value).assign
        )
    }
    val editInput                           = EditConstraintSetInput(
      imageQuality = constraints.imageQuality.assign,
      cloudExtinction = constraints.cloudExtinction.assign,
      skyBackground = constraints.skyBackground.assign,
      waterVapor = constraints.waterVapor.assign,
      elevationRange = createER.assign
    )
    UpdateConstraintSetMutation.execute[F](obsIds, editInput).void
  }
}
