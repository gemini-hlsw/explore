// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas.odb

import cats.effect.Async
import cats.implicits.*
import clue.FetchClient
import clue.data.syntax.*
import clue.syntax.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ConfigurationRequestWithObsIds
import explore.model.ConstraintGroup
import explore.model.ObsIdSet
import explore.model.Observation
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Group
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import queries.common.ObsQueriesGQL.*
import queries.common.TargetQueriesGQL.SetGuideTargetName

import java.time.Instant
import scala.collection.immutable.SortedMap

object ObsQueries:
  type ConstraintsList = SortedMap[ObsIdSet, ConstraintGroup]

  def updateObservationConstraintSet[F[_]: Async](
    obsIds:      List[Observation.Id],
    constraints: ConstraintSet
  )(using FetchClient[F, ObservationDB]): F[Unit] = {
    val createER: ElevationRangeInput = constraints.elevationRange match
      case ElevationRange.ByAirMass(min, max)   =>
        ElevationRangeInput(airMass =
          // These are actually safe, because min and max in the model are refined [1.0 - 3.0]
          AirMassRangeInput(
            min = PosBigDecimal.unsafeFrom(min.toBigDecimal).assign,
            max = PosBigDecimal.unsafeFrom(max.toBigDecimal).assign
          ).assign
        )
      case ElevationRange.ByHourAngle(min, max) =>
        ElevationRangeInput(hourAngle =
          HourAngleRangeInput(
            minHours = min.toBigDecimal.assign,
            maxHours = max.toBigDecimal.assign
          ).assign
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
      .execute:
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .raiseGraphQLErrors
      .void
  }

  def updateVisualizationTime[F[_]: Async](
    obsIds:          List[Observation.Id],
    observationTime: Option[Instant]
  )(using FetchClient[F, ObservationDB]): F[Unit] = {

    val editInput = ObservationTimesInput(observationTime =
      observationTime.flatMap(Timestamp.fromInstantTruncated).orUnassign
    )

    UpdateObservationTimesMutation[F]
      .execute:
        UpdateObservationsTimesInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .raiseGraphQLErrors
      .void
  }

  def updateVisualizationDuration[F[_]: Async](
    obsIds:              List[Observation.Id],
    observationDuration: Option[TimeSpan]
  )(using FetchClient[F, ObservationDB]): F[Unit] = {

    val editInput = ObservationTimesInput(
      observationDuration = observationDuration.map(_.toInput).orUnassign
    )

    UpdateObservationTimesMutation[F]
      .execute:
        UpdateObservationsTimesInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .raiseGraphQLErrors
      .void
  }

  def updateVisualizationTimeAndDuration[F[_]: Async](
    obsIds:              List[Observation.Id],
    observationTime:     Option[Instant],
    observationDuration: Option[TimeSpan]
  )(using FetchClient[F, ObservationDB]): F[Unit] = {
    val editInput =
      ObservationTimesInput(
        observationTime = observationTime.flatMap(Timestamp.fromInstantTruncated).orUnassign,
        observationDuration = observationDuration.map(_.toInput).orUnassign
      )

    UpdateObservationTimesMutation[F]
      .execute:
        UpdateObservationsTimesInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .raiseGraphQLErrors
      .void
  }

  def updatePosAngle[F[_]: Async](
    obsIds:             List[Observation.Id],
    posAngleConstraint: PosAngleConstraint
  )(using FetchClient[F, ObservationDB]): F[Unit] = {

    val editInput =
      ObservationPropertiesInput(posAngleConstraint = posAngleConstraint.toInput.assign)

    UpdateObservationMutation[F]
      .execute:
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .raiseGraphQLErrors
      .void
  }

  def updateNotes[F[_]: Async](
    obsIds: List[Observation.Id],
    notes:  Option[NonEmptyString]
  )(using FetchClient[F, ObservationDB]): F[Unit] = {

    val editInput =
      ObservationPropertiesInput(observerNotes = notes.orUnassign)

    UpdateObservationMutation[F]
      .execute:
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .raiseGraphQLErrors
      .void
  }

  def createObservation[F[_]: Async](
    programId: Program.Id,
    parentId:  Option[Group.Id]
  )(using FetchClient[F, ObservationDB]): F[Observation] =
    ProgramCreateObservation[F]
      .execute:
        CreateObservationInput(
          programId = programId.assign,
          SET = parentId
            .map(gId => ObservationPropertiesInput(groupId = gId.assign))
            .orIgnore
        )
      .raiseGraphQLErrorsOnNoData
      .map: result =>
        result.createObservation.observation

  def createObservationWithTargets[F[_]: Async](
    programId: Program.Id,
    targetIds: Set[Target.Id]
  )(using
    FetchClient[F, ObservationDB]
  ): F[Observation] =
    ProgramCreateObservation[F]
      .execute:
        CreateObservationInput(
          programId = programId.assign,
          SET = ObservationPropertiesInput(
            targetEnvironment = TargetEnvironmentInput(asterism = targetIds.toList.assign).assign
          ).assign
        )
      .raiseGraphQLErrors
      .map(_.createObservation.observation)

  def cloneObservation[F[_]: Async](
    obsId:      Observation.Id,
    newGroupId: Option[Group.Id]
  )(using
    FetchClient[F, ObservationDB]
  ): F[Observation] =
    CloneObservationMutation[F]
      .execute:
        CloneObservationInput(
          observationId = obsId.assign,
          SET = ObservationPropertiesInput(groupId = newGroupId.orUnassign).assign
        )
      .raiseGraphQLErrorsOnNoData
      .map(_.cloneObservation.newObservation)

  def applyObservation[F[_]: Async](
    obsId:           Observation.Id,
    onTargets:       Option[List[Target.Id]] = none,
    onConstraintSet: Option[ConstraintSet] = none,
    onTimingWindows: Option[List[TimingWindow]] = none
  )(using
    FetchClient[F, ObservationDB]
  ): F[Observation] =
    CloneObservationMutation[F]
      .execute:
        CloneObservationInput(
          observationId = obsId.assign,
          SET = ObservationPropertiesInput(
            targetEnvironment =
              onTargets.map(tids => TargetEnvironmentInput(asterism = tids.assign)).orIgnore,
            constraintSet = onConstraintSet.map(_.toInput).orIgnore,
            timingWindows = onTimingWindows.map(_.map(_.toInput)).orIgnore,
            attachments = List.empty.assign // Always clean observation attachments
          ).assign
        )
      .raiseGraphQLErrors
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
      .execute:
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = ObservationPropertiesInput(existence = Existence.Deleted.assign)
        )
      .raiseGraphQLErrors
      .void

  def undeleteObservations[F[_]: Async](
    obsIds: List[Observation.Id]
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    UpdateObservationMutation[F]
      .execute:
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = ObservationPropertiesInput(existence = Existence.Present.assign),
          includeDeleted = true.assign
        )
      .raiseGraphQLErrors
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
    groupIndex: NonNegShort
  )(using FetchClient[F, ObservationDB]) =
    val input = UpdateObservationsInput(
      WHERE = obsId.toWhereObservation.assign,
      SET = ObservationPropertiesInput(
        groupId = groupId.orUnassign,
        groupIndex = groupIndex.assign
      )
    )
    UpdateObservationMutation[F].execute(input).void

  def setGuideTargetName[F[_]: Async](
    obsId:      Observation.Id,
    targetName: Option[NonEmptyString]
  )(using FetchClient[F, ObservationDB]) =
    val input = SetGuideTargetNameInput(
      observationId = obsId.assign,
      targetName = targetName.orUnassign
    )
    SetGuideTargetName[F].execute(input).void

  def createConfigurationRequest[F[_]: Async](
    obsId:         Observation.Id,
    justification: Option[NonEmptyString]
  )(using FetchClient[F, ObservationDB]): F[ConfigurationRequestWithObsIds] =
    val input = CreateConfigurationRequestInput(
      observationId = obsId.assign,
      SET = ConfigurationRequestProperties(justification = justification.orIgnore).assign
    )
    CreateConfigurationRequestMutation[F].execute(input).raiseGraphQLErrors.map(_._1)
