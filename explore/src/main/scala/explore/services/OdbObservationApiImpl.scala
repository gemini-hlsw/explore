// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.effect.Async
import cats.effect.Resource
import cats.implicits.*
import clue.StreamingClient
import clue.data.Input
import clue.data.syntax.*
import clue.syntax.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ConfigurationRequestWithObsIds
import explore.model.Execution
import explore.model.ExecutionOffsets
import explore.model.Observation
import explore.utils.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Group
import lucuma.core.model.ObservationReference
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import queries.common.ObsQueriesGQL.*
import queries.common.ProgramSummaryQueriesGQL.AllProgramObservations
import queries.common.ProgramSummaryQueriesGQL.ObservationExecutionQuery
import queries.common.ProgramSummaryQueriesGQL.ObservationsWorkflowQuery
import queries.common.TargetQueriesGQL.SetGuideTargetName

import java.time.Instant
import scala.concurrent.duration.*

trait OdbObservationApiImpl[F[_]: Async](using StreamingClient[F, ObservationDB])
    extends OdbObservationApi[F]:
  self: OdbApiHelper[F] =>

  def updateObservations(input: UpdateObservationsInput): F[Unit] =
    UpdateObservationMutation[F]
      .execute(input)
      .processErrors
      .void

  def updateObservations(
    obsIds: List[Observation.Id],
    input:  ObservationPropertiesInput
  ): F[Unit] =
    updateObservations:
      UpdateObservationsInput(
        WHERE = obsIds.toWhereObservation.assign,
        SET = input
      )

  def updateObservationConstraintSet(
    obsIds:      List[Observation.Id],
    constraints: ConstraintSet
  ): F[Unit] = {
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

    updateObservations(obsIds, editInput)
  }

  def updateVisualizationTime(
    obsIds:          List[Observation.Id],
    observationTime: Option[Instant]
  ): F[Unit] = {
    val editInput = ObservationTimesInput(observationTime =
      observationTime.flatMap(Timestamp.fromInstantTruncated).orUnassign
    )

    UpdateObservationTimesMutation[F]
      .execute:
        UpdateObservationsTimesInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .processErrors
      .void
  }

  def updateVisualizationDuration(
    obsIds:              List[Observation.Id],
    observationDuration: Option[TimeSpan]
  ): F[Unit] = {
    val editInput = ObservationTimesInput(
      observationDuration = observationDuration.map(_.toInput).orUnassign
    )

    UpdateObservationTimesMutation[F]
      .execute:
        UpdateObservationsTimesInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .processErrors
      .void
  }

  def updateVisualizationTimeAndDuration(
    obsIds:              List[Observation.Id],
    observationTime:     Option[Instant],
    observationDuration: Option[TimeSpan]
  ): F[Unit] = {
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
      .processErrors
      .void
  }

  def updatePosAngle(
    obsIds:             List[Observation.Id],
    posAngleConstraint: PosAngleConstraint
  ): F[Unit] = {
    val editInput =
      ObservationPropertiesInput(posAngleConstraint = posAngleConstraint.toInput.assign)

    UpdateObservationMutation[F]
      .execute:
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .processErrors
      .void
  }

  def updateNotes(
    obsIds: List[Observation.Id],
    notes:  Option[NonEmptyString]
  ): F[Unit] = {
    val editInput = ObservationPropertiesInput(observerNotes = notes.orUnassign)

    UpdateObservationMutation[F]
      .execute:
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = editInput
        )
      .processErrors
      .void
  }

  def createObservation(
    programId: Program.Id,
    parentId:  Option[Group.Id]
  ): F[Observation] =
    ProgramCreateObservation[F]
      .execute:
        CreateObservationInput(
          programId = programId.assign,
          SET = parentId
            .map(gId => ObservationPropertiesInput(groupId = gId.assign))
            .orIgnore
        )
      .processNoDataErrors
      .map: result =>
        result.createObservation.observation

  def createObservationWithTargets(
    programId: Program.Id,
    targetIds: Set[Target.Id]
  ): F[Observation] =
    ProgramCreateObservation[F]
      .execute:
        CreateObservationInput(
          programId = programId.assign,
          SET = ObservationPropertiesInput(
            targetEnvironment = TargetEnvironmentInput(asterism = targetIds.toList.assign).assign
          ).assign
        )
      .processErrors
      .map(_.createObservation.observation)

  def cloneObservation(
    obsId:      Observation.Id,
    newGroupId: Option[Group.Id]
  ): F[Observation] =
    CloneObservationMutation[F]
      .execute:
        CloneObservationInput(
          observationId = obsId.assign,
          SET = ObservationPropertiesInput(groupId = newGroupId.orUnassign).assign
        )
      .processNoDataErrors
      .map(_.cloneObservation.newObservation)

  def applyObservation(
    obsId:           Observation.Id,
    onTargets:       Option[List[Target.Id]] = none,
    onConstraintSet: Option[ConstraintSet] = none,
    onTimingWindows: Option[List[TimingWindow]] = none
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
      .processErrors
      .map(_.cloneObservation.newObservation)

  def deleteObservation(obsId: Observation.Id): F[Unit] =
    deleteObservations(List(obsId))

  def undeleteObservation(obsId: Observation.Id): F[Unit] =
    undeleteObservations(List(obsId))

  def deleteObservations(obsIds: List[Observation.Id]): F[Unit] =
    UpdateObservationMutation[F]
      .execute:
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = ObservationPropertiesInput(existence = Existence.Deleted.assign)
        )
      .processErrors
      .void

  def undeleteObservations(obsIds: List[Observation.Id]): F[Unit] =
    UpdateObservationMutation[F]
      .execute:
        UpdateObservationsInput(
          WHERE = obsIds.toWhereObservation.assign,
          SET = ObservationPropertiesInput(existence = Existence.Present.assign),
          includeDeleted = true.assign
        )
      .processErrors
      .void

  /**
   * @param programId
   * @param obsId
   * @param groupId
   *   Group to move to. `None` to move to top level
   * @param groupIndex
   *   New index in group. `None` to leave position unchanged
   */
  def moveObservation(
    obsId:      Observation.Id,
    groupId:    Option[Group.Id],
    groupIndex: NonNegShort
  ): F[Unit] =
    val input = UpdateObservationsInput(
      WHERE = obsId.toWhereObservation.assign,
      SET = ObservationPropertiesInput(
        groupId = groupId.orUnassign,
        groupIndex = groupIndex.assign
      )
    )
    UpdateObservationMutation[F].execute(input).processErrors.void

  def setGuideTargetName(
    obsId:      Observation.Id,
    targetName: Option[NonEmptyString]
  ): F[Unit] =
    val input = SetGuideTargetNameInput(
      observationId = obsId.assign,
      targetName = targetName.orUnassign
    )
    SetGuideTargetName[F].execute(input).processErrors.void

  def createConfigurationRequest(
    obsId:         Observation.Id,
    justification: Option[NonEmptyString]
  ): F[ConfigurationRequestWithObsIds] =
    val input = CreateConfigurationRequestInput(
      observationId = obsId.assign,
      SET = ConfigurationRequestProperties(justification = justification.orIgnore).assign
    )
    CreateConfigurationRequestMutation[F].execute(input).processErrors.map(_._1)

  def updateConfiguration(
    obsId:              Observation.Id,
    observingMode:      Input[ObservingModeInput],
    posAngleConstraint: Input[PosAngleConstraintInput] = Input.ignore
  ): F[Option[ObservingMode]] =
    val input = UpdateObservationsInput(
      WHERE = obsId.toWhereObservation.assign,
      SET = ObservationPropertiesInput(
        observingMode = observingMode,
        posAngleConstraint = posAngleConstraint
      )
    )

    UpdateConfigurationMutation[F]
      .execute(input)
      .processErrors
      .map(_.updateObservations.observations.headOption.flatMap(_.observingMode))

  def setObservationWorkflowState(obsId: Observation.Id, st: ObservationWorkflowState): F[Unit] =
    SetObservationWorkflowStateMutation[F]
      .execute:
        SetObservationWorkflowStateInput(obsId, st)
      .processErrors
      .void

  def resolveObservationReference(
    obsRef: ObservationReference
  ): F[Option[(Program.Id, Observation.Id)]] =
    ResolveObsReference[F]
      .query(obsRef.assign)
      .processErrors
      .map(_.observation.map(r => (r.program.id, r.id)))

  def sequenceOffsets(obsId: Observation.Id): F[Option[ExecutionOffsets]] =
    SequenceOffsets[F]
      .query(obsId)
      .raiseGraphQLErrors
      .map(_.observation.map(_.execution))

  def observationEditSubscription(
    obsId: Observation.Id
  ): Resource[F, fs2.Stream[F, Unit]] =
    ObservationEditSubscription
      .subscribe[F](obsId.toObservationEditInput)
      .raiseFirstNoDataError
      .ignoreGraphQLErrors
      .map: // TODO Do we want throttle in all subscriptions? Parametrize throttle timeout?
        _.void.throttle(5.seconds)

  def programObservationsDeltaSubscription(
    programId: Program.Id
  ): Resource[F, fs2.Stream[F, ProgramObservationsDelta.Data.ObservationEdit]] =
    ProgramObservationsDelta
      .subscribe[F](programId.toObservationEditInput)
      .processErrors("ProgramObservationsDelta")
      .map(_.map(_.observationEdit))

  def observationExecution(obsId: Observation.Id): F[Option[Execution]] =
    ObservationExecutionQuery[F]
      .query(obsId)
      .raiseGraphQLErrorsOnNoData
      .map(_.observation.map(_.execution))

  def allProgramObservations(programId: Program.Id): F[List[Observation]] =
    drain[Observation, Observation.Id, AllProgramObservations.Data.Observations](
      offset =>
        AllProgramObservations[F]
          .query(programId.toWhereObservation, offset.orUnassign)
          // We need this because we currently get errors for things like having no targets
          .processNoDataErrors
          .map(_.observations),
      _.matches,
      _.hasMore,
      _.id
    )

  def observationWorkflows(
    whereObservation: WhereObservation
  ): F[List[ObservationsWorkflowQuery.Data.Observations.Matches]] =
    drain[
      ObservationsWorkflowQuery.Data.Observations.Matches,
      Observation.Id,
      ObservationsWorkflowQuery.Data.Observations
    ](
      offset =>
        ObservationsWorkflowQuery[F]
          .query(whereObservation, offset.orUnassign)
          .processErrors
          .map(_.observations),
      _.matches,
      _.hasMore,
      _.id
    )

  def obsCalcSubscription(
    programId: Program.Id
  ): Resource[F, fs2.Stream[F, ObsCalcSubscription.Data.ObscalcUpdate]] =
    ObsCalcSubscription
      .subscribe[F](ObscalcUpdateInput(programId.assign))
      .processErrors("ObsCalcSubscription")
      .map(_.map(_.obscalcUpdate))
