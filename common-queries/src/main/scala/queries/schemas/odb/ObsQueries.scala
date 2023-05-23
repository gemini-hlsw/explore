// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas.odb

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.effect.Async
import cats.implicits.*
import clue.FetchClient
import clue.data.syntax.*
import crystal.Pot
import crystal.implicits.given
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import explore.DefaultErrorPolicy
import explore.data.KeyedIndexedList
import explore.model.ConstraintGroup
import explore.model.ObsIdSet
import explore.model.ObsSummary
import explore.model.OdbItcResult
import explore.model.syntax.all.*
import explore.optics.all.*
import japgolly.scalajs.react.*
import lucuma.core.math.Offset
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ExposureTimeMode.FixedExposureMode
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
import monocle.Iso
import monocle.Lens
import queries.common.ObsQueriesGQL.*

import java.time.Duration
import java.time.Instant
import scala.collection.immutable.SortedMap

object ObsQueries:
  type ObservationList = KeyedIndexedList[Observation.Id, ObsSummary]
  type ConstraintsList = SortedMap[ObsIdSet, ConstraintGroup]

  extension (self: OdbItcResult.Success)
    def asFixedExposureTime: FixedExposureMode =
      FixedExposureMode(NonNegInt.unsafeFrom(self.exposures.value), self.exposureTime)

  def updateObservationConstraintSet[F[_]: Async](
    programId:   Program.Id,
    obsIds:      List[Observation.Id],
    constraints: ConstraintSet
  )(using FetchClient[F, ObservationDB]): F[Unit] = {
    val createER: ElevationRangeInput = constraints.elevationRange match
      case ElevationRange.AirMass(min, max)   =>
        ElevationRangeInput(airMass =
          // These are actually safe, because min and max in the model are refined [1.0 - 3.0]
          AirMassRangeInput(
            min = PosBigDecimal.unsafeFrom(min.value).assign,
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
    UpdateObservationMutation[F]
      .execute(
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
  )(using FetchClient[F, ObservationDB]): F[Unit] = {

    val editInput = ObservationPropertiesInput(visualizationTime =
      visualizationTime.flatMap(Timestamp.fromInstantTruncated).orUnassign
    )

    UpdateObservationMutation[F]
      .execute(
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
  )(using FetchClient[F, ObservationDB]): F[Unit] = {

    val editInput =
      ObservationPropertiesInput(posAngleConstraint = posAngleConstraint.toInput.assign)

    UpdateObservationMutation[F]
      .execute(
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
  )(using FetchClient[F, ObservationDB]): F[ObsSummary] =
    ProgramCreateObservation[F]
      .execute(CreateObservationInput(programId = programId))
      .map(_.createObservation.observation)

  def createObservationWithTargets[F[_]: Async](
    programId: Program.Id,
    targetIds: Set[Target.Id]
  )(using
    FetchClient[F, ObservationDB]
  ): F[ObsSummary] =
    ProgramCreateObservation[F]
      .execute(
        CreateObservationInput(
          programId = programId,
          SET = ObservationPropertiesInput(
            targetEnvironment = TargetEnvironmentInput(asterism = targetIds.toList.assign).assign
          ).assign
        )
      )
      .map(_.createObservation.observation)

  def cloneObservation[F[_]: Async](
    obsId: Observation.Id
  )(using
    FetchClient[F, ObservationDB]
  ): F[ObsSummary] =
    CloneObservationMutation[F]
      .execute(CloneObservationInput(observationId = obsId))
      .map(_.cloneObservation.newObservation)

  def applyObservation[F[_]: Async](
    obsId:     Observation.Id,
    targetIds: List[Target.Id]
  )(using
    FetchClient[F, ObservationDB]
  ): F[ObsSummary] =
    CloneObservationMutation[F]
      .execute(
        CloneObservationInput(
          observationId = obsId,
          SET = ObservationPropertiesInput(targetEnvironment =
            TargetEnvironmentInput(asterism = targetIds.assign).assign
          ).assign
        )
      )
      .map(_.cloneObservation.newObservation)

  def deleteObservation[F[_]: Async](
    programId: Program.Id,
    obsId:     Observation.Id
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    deleteObservations(programId, List(obsId))

  def undeleteObservation[F[_]: Async](
    programId: Program.Id,
    obsId:     Observation.Id
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    undeleteObservations(programId, List(obsId))

  def deleteObservations[F[_]: Async](
    programId: Program.Id,
    obsIds:    List[Observation.Id]
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    UpdateObservationMutation[F]
      .execute(
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
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    UpdateObservationMutation[F]
      .execute(
        UpdateObservationsInput(
          programId = programId,
          WHERE = obsIds.toWhereObservation.assign,
          SET = ObservationPropertiesInput(existence = Existence.Present.assign),
          includeDeleted = true.assign
        )
      )
      .void
