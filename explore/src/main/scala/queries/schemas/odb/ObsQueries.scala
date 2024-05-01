// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas.odb

import cats.effect.Async
import cats.implicits.*
import clue.ErrorPolicy
import clue.FetchClient
import clue.data.syntax.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.data.KeyedIndexedList
import explore.model.ConstraintGroup
import explore.model.ObsIdSet
import explore.model.ObsSummary
import explore.model.OdbItcResult
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ExposureTimeMode.FixedExposureMode
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import queries.common.ObsQueriesGQL.*

import java.time.Instant
import scala.collection.immutable.SortedMap

object ObsQueries:
  type ObservationList = KeyedIndexedList[Observation.Id, ObsSummary]
  type ConstraintsList = SortedMap[ObsIdSet, ConstraintGroup]

  private given ErrorPolicy.IgnoreOnData.type = ErrorPolicy.IgnoreOnData

  extension (self: OdbItcResult.Success)
    def asFixedExposureTime: FixedExposureMode =
      FixedExposureMode(PosInt.unsafeFrom(self.sciExposures.value), self.sciExposureTime)

  def updateObservationConstraintSet[F[_]: Async](
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
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      )
      .void
  }

  def updateVisualizationTime[F[_]: Async](
    obsIds:            List[Observation.Id],
    visualizationTime: Option[Instant]
  )(using FetchClient[F, ObservationDB]): F[Unit] = {

    val editInput = ObservationPropertiesInput(visualizationTime =
      visualizationTime.flatMap(Timestamp.fromInstantTruncated).orUnassign
    )

    UpdateObservationMutation[F]
      .execute(
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      )
      .void
  }

  def updatePosAngle[F[_]: Async](
    obsIds:             List[Observation.Id],
    posAngleConstraint: PosAngleConstraint
  )(using FetchClient[F, ObservationDB]): F[Unit] = {

    val editInput =
      ObservationPropertiesInput(posAngleConstraint = posAngleConstraint.toInput.assign)

    UpdateObservationMutation[F]
      .execute(
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      )
      .void
  }

  def createObservation[F[_]: Async](
    programId: Program.Id,
    parentId:  Option[Group.Id]
  )(using FetchClient[F, ObservationDB]): F[ObsSummary] =
    ProgramCreateObservation[F]
      .execute(
        CreateObservationInput(
          programId = programId.assign,
          SET = parentId
            .map(gId => ObservationPropertiesInput(groupId = gId.assign))
            .orIgnore
        )
      )
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
          programId = programId.assign,
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
      .execute(CloneObservationInput(observationId = obsId.assign))
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
          observationId = obsId.assign,
          SET = ObservationPropertiesInput(targetEnvironment =
            TargetEnvironmentInput(asterism = targetIds.assign).assign
          ).assign
        )
      )
      .map(_.cloneObservation.newObservation)

  def deleteObservation[F[_]: Async](
    obsId: Observation.Id
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    deleteObservations(List(obsId))

  def undeleteObservation[F[_]: Async](
    obsId: Observation.Id
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    undeleteObservations(List(obsId))

  def deleteObservations[F[_]: Async](
    obsIds: List[Observation.Id]
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    UpdateObservationMutation[F]
      .execute(
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = ObservationPropertiesInput(existence = Existence.Deleted.assign)
        )
      )
      .void

  def undeleteObservations[F[_]: Async](
    obsIds: List[Observation.Id]
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    UpdateObservationMutation[F]
      .execute(
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = ObservationPropertiesInput(existence = Existence.Present.assign),
          includeDeleted = true.assign
        )
      )
      .void

  /**
   * @param programId
   * @param obsId
   * @param groupId
   *   Group to move to. `None` to move to top level
   * @param groupIndex
   *   New index in group. `None` to leave position unchanged
   */
  def moveObservation[F[_]: Async](
    obsId:      Observation.Id,
    groupId:    Option[Group.Id],
    groupIndex: Option[NonNegShort]
  )(using FetchClient[F, ObservationDB]) =
    val input = UpdateObservationsInput(
      WHERE = obsId.toWhereObservation.assign,
      SET = ObservationPropertiesInput(
        groupId = groupId.orUnassign,
        groupIndex = groupIndex.orIgnore
      )
    )
    UpdateObservationMutation[F].execute(input).void
