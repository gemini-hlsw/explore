// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.Async
import cats.implicits._
import clue.TransactionalClient
import clue.data.syntax._
import crystal.Pot
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import explore.data.KeyedIndexedList
import explore.model.ConstraintGroup
import explore.model.ObsIdSet
import explore.model.ObsSummaryWithTitleAndConstraints
import explore.model.ObsSummaryWithTitleConstraintsAndConf
import explore.model.ScienceMode
import explore.model.TargetSummary
import explore.optics.all._
import explore.model.syntax.all._
import japgolly.scalajs.react._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import monocle.Focus
import monocle.Getter
import monocle.Lens
import queries.common.ObsQueriesGQL._
import queries.schemas.implicits._

import java.time.Instant
import scala.collection.immutable.SortedMap
import lucuma.core.model.ExposureTimeMode.FixedExposure

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

  type ITCSuccess = ObservationData.Itc
  val ITCSuccess = ObservationData.Itc

  case class ScienceData(
    requirements: ScienceRequirementsData,
    mode:         Option[ScienceMode],
    constraints:  ConstraintSet,
    targets:      Targets,
    posAngle:     Option[PosAngleConstraint],
    potITC:       Pot[Option[ITCSuccess]]
  )

  object ScienceData {
    val requirements: Lens[ScienceData, ScienceRequirementsData] =
      Focus[ScienceData](_.requirements)
    val mode: Lens[ScienceData, Option[ScienceMode]]             =
      Focus[ScienceData](_.mode)
    val targets: Lens[ScienceData, Targets]                      =
      Focus[ScienceData](_.targets)
    val constraints: Lens[ScienceData, ConstraintSet]            =
      Focus[ScienceData](_.constraints)
    val posAngle: Lens[ScienceData, Option[PosAngleConstraint]]  =
      Focus[ScienceData](_.posAngle)
    val potITC: Lens[ScienceData, Pot[Option[ITCSuccess]]]       =
      Focus[ScienceData](_.potITC)
  }

  case class ObsEditData(
    id:                Observation.Id,
    title:             String,
    subtitle:          Option[NonEmptyString],
    visualizationTime: Option[Instant],
    scienceData:       ScienceData,
    itcExposureTime:   Option[FixedExposure]
  )

  object ObsEditData {
    val title: Lens[ObsEditData, String]                      = Focus[ObsEditData](_.title)
    val subtitle: Lens[ObsEditData, Option[NonEmptyString]]   = Focus[ObsEditData](_.subtitle)
    val visualizationTime: Lens[ObsEditData, Option[Instant]] =
      Focus[ObsEditData](_.visualizationTime)
    val scienceData: Lens[ObsEditData, ScienceData]           = Focus[ObsEditData](_.scienceData)
  }

  case class ObsSummariesWithConstraints(
    observations:     ObservationList,
    constraintGroups: ConstraintsList,
    targetMap:        SortedMap[Target.Id, TargetSummary]
  )

  object ObsSummariesWithConstraints {
    val observations     = Focus[ObsSummariesWithConstraints](_.observations)
    val constraintGroups = Focus[ObsSummariesWithConstraints](_.constraintGroups)
  }

  extension (self: ObsEditQuery.Data.type)
    def asObsEditData: Getter[ObsEditQuery.Data, Option[ObsEditData]] =
      data =>
        data.observation.map { obs =>
          ObsEditData(
            id = obs.id,
            title = obs.title,
            subtitle = obs.subtitle,
            visualizationTime = obs.visualizationTime,
            itcExposureTime = obs.itc.map(_.asFixedExposureTime),
            scienceData = ScienceData(
              requirements = obs.scienceRequirements,
              mode = obs.scienceMode,
              constraints = obs.constraintSet,
              targets = obs.targetEnvironment,
              posAngle = obs.posAngleConstraint,
              potITC = Pot(obs.itc)
            )
          )
        }

  extension (self: ITCSuccess)
    def asFixedExposureTime = FixedExposure(self.exposures, self.exposureTime)

  extension (self: ProgramObservationsQuery.Data.type)
    def asObsSummariesWithConstraints
      : Getter[ProgramObservationsQuery.Data, ObsSummariesWithConstraints] =
      data =>
        ObsSummariesWithConstraints(
          KeyedIndexedList.fromList(
            data.observations.matches.map(mtch =>
              ObsSummaryWithTitleConstraintsAndConf(
                mtch.id,
                mtch.title,
                mtch.subtitle,
                mtch.constraintSet,
                mtch.status,
                mtch.activeStatus,
                mtch.plannedTime.execution,
                mtch.scienceMode,
                mtch.visualizationTime
              )
            ),
            ObsSummaryWithTitleConstraintsAndConf.id.get
          ),
          data.constraintSetGroup.matches.toSortedMap(ConstraintGroup.obsIds.get),
          data.targetGroup.matches
            .toSortedMap(
              _.target.id,
              group => TargetSummary(group.observationIds.toSet, group.target.id)
            )
        )

  def updateObservationConstraintSet[F[_]: Async](
    obsIds:      List[Observation.Id],
    constraints: ConstraintSet
  )(implicit
    c:           TransactionalClient[F, ObservationDB]
  ): F[Unit] = {
    val createER: ElevationRangeInput = constraints.elevationRange match
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

    val editInput = ObservationPropertiesInput(
      constraintSet = ConstraintSetInput(
        imageQuality = constraints.imageQuality.assign,
        cloudExtinction = constraints.cloudExtinction.assign,
        skyBackground = constraints.skyBackground.assign,
        waterVapor = constraints.waterVapor.assign,
        elevationRange = createER.assign
      ).assign
    )
    UpdateObservationMutation
      .execute[F](
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      )
      .void
  }

  def updateVisualizationTime[F[_]: Async](
    obsIds:            List[Observation.Id],
    visualizationTime: Option[Instant]
  )(using TransactionalClient[F, ObservationDB]): F[Unit] = {

    val editInput = ObservationPropertiesInput(visualizationTime = visualizationTime.orUnassign)

    UpdateObservationMutation
      .execute[F](
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      )
      .void
  }

  def updatePosAngle[F[_]: Async](
    obsIds:             List[Observation.Id],
    posAngleConstraint: Option[PosAngleConstraint]
  )(using TransactionalClient[F, ObservationDB]): F[Unit] = {

    val editInput =
      ObservationPropertiesInput(posAngleConstraint = posAngleConstraint.map(_.toInput).orUnassign)

    UpdateObservationMutation
      .execute[F](
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      )
      .void
  }
  def createObservation[F[_]: Async](
    programId: Program.Id
  )(using TransactionalClient[F, ObservationDB]): F[ObsSummaryWithTitleAndConstraints] =
    ProgramCreateObservation.execute[F](CreateObservationInput(programId = programId)).map { data =>
      val obs = data.createObservation.observation
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

  def deleteObservation[F[_]: Async](
    obsId: Observation.Id
  )(using TransactionalClient[F, ObservationDB]): F[Unit] =
    ProgramDeleteObservations
      .execute[F](
        DeleteObservationsInput(WHERE = obsId.toWhereObservation.assign)
      )
      .void

  def undeleteObservation[F[_]: Async](
    obsId: Observation.Id
  )(using TransactionalClient[F, ObservationDB]): F[Unit] =
    ProgramUndeleteObservations
      .execute[F](
        UndeleteObservationsInput(WHERE = obsId.toWhereObservation.assign)
      )
      .void
}
