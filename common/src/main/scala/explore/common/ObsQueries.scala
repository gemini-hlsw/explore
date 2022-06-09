// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.Async
import cats.implicits._
import clue.TransactionalClient
import clue.data.syntax._
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.ConstraintGroup
import explore.model.ObsIdSet
import explore.model.ObsSummaryWithTitleAndConstraints
import explore.model.ObsSummaryWithTitleConstraintsAndConf
import explore.model.ScienceMode
import explore.model.TargetSummary
import explore.model.reusability._
import explore.optics._
import japgolly.scalajs.react._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Getter
import monocle.Lens
import monocle.macros.GenIso
import queries.common.ObsQueriesGQL._
import queries.schemas.implicits._

import java.time.Instant
import scala.collection.immutable.SortedMap

object ObsQueries {

  type ObservationList = KeyedIndexedList[Observation.Id, ObsSummaryWithTitleConstraintsAndConf]
  type ConstraintsList = SortedMap[ObsIdSet, ConstraintGroup]

  type ObservationData = ObsEditQuery.Data.Observation
  val ObservationData = ObsEditQuery.Data.Observation
  type ScienceRequirementsData = ObservationData.ScienceRequirements
  val ScienceRequirementsData = ObservationData.ScienceRequirements
  type Targets                      = ObservationData.TargetEnvironment
  type SpectroscopyRequirementsData = ObservationData.ScienceRequirements.Spectroscopy
  val SpectroscopyRequirementsData = ObservationData.ScienceRequirements.Spectroscopy

  case class ScienceData(
    requirements: ScienceRequirementsData,
    mode:         Option[ScienceMode],
    constraints:  ConstraintSet,
    targets:      Targets,
    posAngle:     Option[PosAngleConstraint]
  )

  object ScienceData {
    val requirements: Lens[ScienceData, ScienceRequirementsData] =
      Focus[ScienceData](_.requirements)
    val mode: Lens[ScienceData, Option[ScienceMode]]             =
      Focus[ScienceData](_.mode)
    val constraints: Lens[ScienceData, ConstraintSet]            =
      Focus[ScienceData](_.constraints)
    val posAngle: Lens[ScienceData, Option[PosAngleConstraint]]  =
      Focus[ScienceData](_.posAngle)
  }

  val scienceDataForObs: Lens[ObservationData, ScienceData] =
    disjointZip(
      ObservationData.scienceRequirements,
      ObservationData.scienceMode,
      ObservationData.constraintSet,
      ObservationData.targetEnvironment,
      ObservationData.posAngleConstraint
    )
      .andThen(GenIso.fields[ScienceData].reverse)

  case class ObsSummariesWithConstraints(
    observations:     ObservationList,
    constraintGroups: ConstraintsList,
    targetMap:        SortedMap[Target.Id, TargetSummary]
  )

  object ObsSummariesWithConstraints {
    val observations     = Focus[ObsSummariesWithConstraints](_.observations)
    val constraintGroups = Focus[ObsSummariesWithConstraints](_.constraintGroups)

    implicit val reusabilityObsSummaryWithConstraints: Reusability[ObsSummariesWithConstraints] =
      Reusability.derive
  }

  private val queryToObsSummariesWithConstraintsGetter
    : Getter[ProgramObservationsQuery.Data, ObsSummariesWithConstraints] = data =>
    ObsSummariesWithConstraints(
      KeyedIndexedList.fromList(
        data.observations.nodes.map(node =>
          ObsSummaryWithTitleConstraintsAndConf(
            node.id,
            node.title,
            node.subtitle,
            node.constraintSet,
            node.status,
            node.activeStatus,
            node.plannedTime.execution,
            node.scienceMode,
            node.visualizationTime
          )
        ),
        ObsSummaryWithTitleConstraintsAndConf.id.get
      ),
      data.constraintSetGroup.nodes.toSortedMap(ConstraintGroup.obsIds.get),
      data.targetGroup.nodes
        .toSortedMap(
          _.target.id,
          group => TargetSummary(group.observationIds.toSet, group.target.id, group.target.sidereal)
        )
    )

  implicit class ProgramObservationsQueryDataOps(val self: ProgramObservationsQuery.Data.type)
      extends AnyVal {
    def asObsSummariesWithConstraints = queryToObsSummariesWithConstraintsGetter
  }

  def updateObservationConstraintSet[F[_]: Async](
    obsIds:      List[Observation.Id],
    constraints: ConstraintSet
  )(implicit
    c:           TransactionalClient[F, ObservationDB]
  ): F[Unit] = {
    val createER: ElevationRangeInput = constraints.elevationRange match {
      case ElevationRange.AirMass(min, max)   =>
        ElevationRangeInput(airMass =
          // These are actually safe, because min and max in the model are refined [1.0 - 3.0]
          AirMassRangeInput(min = PosBigDecimal.unsafeFrom(min.value).assign,
                            max = PosBigDecimal.unsafeFrom(max.value).assign
          ).assign
        )
      case ElevationRange.HourAngle(min, max) =>
        ElevationRangeInput(hourAngle =
          HourAngleRangeInput(minHours = min.value.assign, maxHours = max.value.assign).assign
        )
    }
    val editInput                     = ObservationPropertiesInput(
      constraintSet = ConstraintSetInput(
        imageQuality = constraints.imageQuality.assign,
        cloudExtinction = constraints.cloudExtinction.assign,
        skyBackground = constraints.skyBackground.assign,
        waterVapor = constraints.waterVapor.assign,
        elevationRange = createER.assign
      ).assign
    )
    EditObservationMutation
      .execute[F](
        EditObservationInput(
          select = ObservationSelectInput(observationIds = obsIds.assign),
          patch = editInput
        )
      )
      .void
  }

  def updateVisualizationTime[F[_]: Async](
    obsIds:            List[Observation.Id],
    visualizationTime: Option[Instant]
  )(implicit
    c:                 TransactionalClient[F, ObservationDB]
  ): F[Unit] = {

    val editInput = ObservationPropertiesInput(
      visualizationTime = visualizationTime.orUnassign
    )

    EditObservationMutation
      .execute[F](
        EditObservationInput(
          select = ObservationSelectInput(observationIds = obsIds.assign),
          patch = editInput
        )
      )
      .void
  }

  def updatePosAngle[F[_]: Async](
    obsIds:             List[Observation.Id],
    posAngleConstraint: Option[PosAngleConstraint]
  )(implicit
    c:                  TransactionalClient[F, ObservationDB]
  ): F[Unit] = {

    val editInput = ObservationPropertiesInput(
      posAngleConstraint = posAngleConstraint.map(_.toInput).orUnassign
    )

    EditObservationMutation
      .execute[F](
        EditObservationInput(
          select = ObservationSelectInput(observationIds = obsIds.assign),
          patch = editInput
        )
      )
      .void
  }
  def createObservation[F[_]: Async](programId: Program.Id)(implicit
    c:                                          TransactionalClient[F, ObservationDB]
  ): F[ObsSummaryWithTitleAndConstraints] =
    ProgramCreateObservation.execute[F](CreateObservationInput(programId = programId)).map { data =>
      val obs = data.createObservation
      ObsSummaryWithTitleAndConstraints(
        obs.id,
        obs.title,
        obs.subtitle,
        obs.constraintSet,
        obs.status,
        obs.activeStatus,
        obs.plannedTime.execution
      )
    }

  def deleteObservation[F[_]: Async](obsId: Observation.Id)(implicit
    c:                                      TransactionalClient[F, ObservationDB]
  ): F[Unit] =
    ProgramDeleteObservation
      .execute[F](
        DeleteObservationInput(select = ObservationSelectInput(observationIds = List(obsId).assign))
      )
      .void

  def undeleteObservation[F[_]: Async](obsId: Observation.Id)(implicit
    c:                                        TransactionalClient[F, ObservationDB]
  ): F[Unit] =
    ProgramUndeleteObservation
      .execute[F](
        UndeleteObservationInput(select =
          ObservationSelectInput(observationIds = List(obsId).assign)
        )
      )
      .void
}
