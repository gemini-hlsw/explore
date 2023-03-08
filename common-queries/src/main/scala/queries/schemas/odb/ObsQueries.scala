// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas.odb

import cats.Eq
import cats.derived.*
import cats.effect.Async
import cats.implicits.*
import clue.TransactionalClient
import clue.data.syntax.*
import crystal.Pot
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import explore.data.KeyedIndexedList
import explore.model.ConstraintGroup
import explore.model.ObsIdSet
import explore.model.ObsSummaryWithTitleAndConstraints
import explore.model.ObsSummaryWithTitleConstraintsAndConf
import explore.model.OdbItcResult
import explore.model.TargetSummary
import explore.model.syntax.all.*
import explore.optics.all.*
import japgolly.scalajs.react.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ExposureTimeMode.FixedExposure
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.syntax.time.*
import lucuma.core.util.Timestamp
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import monocle.Focus
import monocle.Getter
import monocle.Lens
import queries.common.ObsQueriesGQL.*

import java.time.Duration
import java.time.Instant
import scala.collection.immutable.SortedMap
import lucuma.core.math.Offset
import cats.data.NonEmptyList

object ObsQueries:
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
    requirements:       ScienceRequirementsData,
    mode:               Option[ObservingMode],
    constraints:        ConstraintSet,
    targets:            Targets,
    posAngle:           PosAngleConstraint,
    scienceOffsets:     Option[NonEmptyList[Offset]],
    acquisitionOffsets: Option[NonEmptyList[Offset]],
    potITC:             Pot[Option[OdbItcResult.Success]]
  )

  object ScienceData {
    val requirements: Lens[ScienceData, ScienceRequirementsData]            =
      Focus[ScienceData](_.requirements)
    val mode: Lens[ScienceData, Option[ObservingMode]]                      =
      Focus[ScienceData](_.mode)
    val targets: Lens[ScienceData, Targets]                                 =
      Focus[ScienceData](_.targets)
    val constraints: Lens[ScienceData, ConstraintSet]                       =
      Focus[ScienceData](_.constraints)
    val posAngle: Lens[ScienceData, PosAngleConstraint]                     =
      Focus[ScienceData](_.posAngle)
    val potITC: Lens[ScienceData, Pot[Option[OdbItcResult.Success]]]        =
      Focus[ScienceData](_.potITC)
    val scienceOffsets: Lens[ScienceData, Option[NonEmptyList[Offset]]]     =
      Focus[ScienceData](_.scienceOffsets)
    val acquisitionOffsets: Lens[ScienceData, Option[NonEmptyList[Offset]]] =
      Focus[ScienceData](_.acquisitionOffsets)
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
  ) derives Eq

  object ObsSummariesWithConstraints {
    val observations     = Focus[ObsSummariesWithConstraints](_.observations)
    val constraintGroups = Focus[ObsSummariesWithConstraints](_.constraintGroups)
  }

  extension (data: ObsEditQuery.Data)
    def asObsEditData: Option[ObsEditData] =
      data.observation.map { obs =>
        val itcSuccess = none[OdbItcResult.Success]
        ObsEditData(
          id = obs.id,
          title = obs.title,
          subtitle = obs.subtitle,
          visualizationTime = obs.visualizationTime.map(_.toInstant),
          itcExposureTime = itcSuccess.map(_.asFixedExposureTime),
          scienceData = ScienceData(
            requirements = obs.scienceRequirements,
            mode = obs.observingMode,
            constraints = obs.constraintSet,
            targets = obs.targetEnvironment,
            posAngle = obs.posAngleConstraint,
            scienceOffsets = NonEmptyList.fromList(
              data.sequence.foldMap(_.executionConfig.allScienceOffsets).distinct
            ),
            acquisitionOffsets = NonEmptyList.fromList(
              data.sequence.foldMap(_.executionConfig.allAcquisitionOffsets).distinct
            ),
            potITC = Pot(itcSuccess)
          )
        )
      }

  extension (self: OdbItcResult.Success)
    def asFixedExposureTime: FixedExposure =
      FixedExposure(self.exposures, self.exposureTime)

  extension (data: ProgramObservationsQuery.Data)
    def asObsSummariesWithConstraints: ObsSummariesWithConstraints =
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
              mtch.observingMode,
              none // mtch.visualizationTime
            )
          ),
          ObsSummaryWithTitleConstraintsAndConf.id.get
        ),
        data.constraintSetGroup.matches.toSortedMap(ConstraintGroup.obsIds.get),
        data.targetGroup.matches
          .toSortedMap(
            _.target.id,
            group => TargetSummary(group.observations.matches.map(_.id).toSet, group.target.id)
          )
      )

  def updateObservationConstraintSet[F[_]: Async](
    programId:   Program.Id,
    obsIds:      List[Observation.Id],
    constraints: ConstraintSet
  )(using TransactionalClient[F, ObservationDB]): F[Unit] = {
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
          programId = programId,
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      )
      .void
  }

  def updateVisualizationTime[F[_]: Async](
    programId:         Program.Id,
    obsIds:            List[Observation.Id],
    visualizationTime: Option[Instant]
  )(using TransactionalClient[F, ObservationDB]): F[Unit] = {

    val editInput = ObservationPropertiesInput(visualizationTime =
      visualizationTime.flatMap(Timestamp.fromInstantTruncated).orUnassign
    )

    UpdateObservationMutation
      .execute[F](
        UpdateObservationsInput(
          programId = programId,
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      )
      .void
  }

  def updatePosAngle[F[_]: Async](
    programId:          Program.Id,
    obsIds:             List[Observation.Id],
    posAngleConstraint: PosAngleConstraint
  )(using TransactionalClient[F, ObservationDB]): F[Unit] = {

    val editInput =
      ObservationPropertiesInput(posAngleConstraint = posAngleConstraint.toInput.assign)

    UpdateObservationMutation
      .execute[F](
        UpdateObservationsInput(
          programId = programId,
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

  def createObservationWithTargets[F[_]: Async](
    programId: Program.Id,
    targetIds: Set[Target.Id]
  )(using TransactionalClient[F, ObservationDB]): F[ObsSummaryWithTitleAndConstraints] =
    ProgramCreateObservation
      .execute[F](
        CreateObservationInput(
          programId = programId,
          SET = ObservationPropertiesInput(
            targetEnvironment = TargetEnvironmentInput(asterism = targetIds.toList.assign).assign
          ).assign
        )
      )
      .map { data =>
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

  def cloneObservation[F[_]: Async](
    obsId: Observation.Id
  )(using TransactionalClient[F, ObservationDB]): F[ObsSummaryWithTitleAndConstraints] =
    CloneObservationMutation
      .execute[F](CloneObservationInput(observationId = obsId))
      .map { o =>
        val newObs = o.cloneObservation.newObservation
        ObsSummaryWithTitleAndConstraints(
          newObs.id,
          newObs.title,
          newObs.subtitle,
          newObs.constraintSet,
          newObs.status,
          newObs.activeStatus,
          newObs.plannedTime.execution
        )
      }

  def applyObservation[F[_]: Async](
    obsId:     Observation.Id,
    targetIds: List[Target.Id]
  )(using TransactionalClient[F, ObservationDB]): F[ObsSummaryWithTitleAndConstraints] =
    CloneObservationMutation
      .execute[F](
        CloneObservationInput(
          observationId = obsId,
          SET = ObservationPropertiesInput(targetEnvironment =
            TargetEnvironmentInput(asterism = targetIds.assign).assign
          ).assign
        )
      )
      .map { o =>
        val newObs = o.cloneObservation.newObservation
        ObsSummaryWithTitleAndConstraints(
          newObs.id,
          newObs.title,
          newObs.subtitle,
          newObs.constraintSet,
          newObs.status,
          newObs.activeStatus,
          newObs.plannedTime.execution
        )
      }

  def deleteObservation[F[_]: Async](
    programId: Program.Id,
    obsId:     Observation.Id
  )(using TransactionalClient[F, ObservationDB]): F[Unit] =
    deleteObservations(programId, List(obsId))

  def undeleteObservation[F[_]: Async](
    programId: Program.Id,
    obsId:     Observation.Id
  )(using TransactionalClient[F, ObservationDB]): F[Unit] =
    undeleteObservations(programId, List(obsId))

  def deleteObservations[F[_]: Async](
    programId: Program.Id,
    obsIds:    List[Observation.Id]
  )(using TransactionalClient[F, ObservationDB]): F[Unit] =
    UpdateObservationMutation
      .execute[F](
        UpdateObservationsInput(
          programId = programId,
          WHERE = obsIds.toWhereObservation.assign,
          SET = ObservationPropertiesInput(existence = Existence.Deleted.assign)
        )
      )
      .void

  def undeleteObservations[F[_]: Async](
    programId: Program.Id,
    obsIds:    List[Observation.Id]
  )(using TransactionalClient[F, ObservationDB]): F[Unit] =
    UpdateObservationMutation
      .execute[F](
        UpdateObservationsInput(
          programId = programId,
          WHERE = obsIds.toWhereObservation.assign,
          SET = ObservationPropertiesInput(existence = Existence.Present.assign),
          includeDeleted = true.assign
        )
      )
      .void
