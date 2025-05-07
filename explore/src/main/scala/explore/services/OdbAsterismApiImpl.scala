// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import cats.implicits.*
import clue.FetchClient
import clue.data.syntax.*
import clue.model.GraphQLResponse.*
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import queries.common.AsterismQueriesGQL.*
import queries.common.ObsQueriesGQL.*

trait OdbAsterismApiImpl[F[_]: MonadThrow](using FetchClient[F, ObservationDB])
    extends OdbAsterismApi[F]:
  def replaceAsterism(obsIds: List[Observation.Id], targetIds: List[Target.Id]): F[Unit] =
    val input = UpdateObservationsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = ObservationPropertiesInput(
        targetEnvironment = TargetEnvironmentInput(asterism = targetIds.assign).assign
      )
    )
    UpdateObservationMutation[F].execute(input).raiseGraphQLErrors.void

  def addTargetsToAsterisms(obsIds: List[Observation.Id], targetIds: List[Target.Id]): F[Unit] =
    val input = UpdateAsterismsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = EditAsterismsPatchInput(ADD = targetIds.assign)
    )
    UpdateAsterismsMutation[F].execute(input).raiseGraphQLErrors.void

  def removeTargetsFromAsterisms(
    obsIds:    List[Observation.Id],
    targetIds: List[Target.Id]
  ): F[Unit] =
    val input = UpdateAsterismsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = EditAsterismsPatchInput(DELETE = targetIds.assign)
    )
    UpdateAsterismsMutation[F].execute(input).raiseGraphQLErrors.void

  def addAndRemoveTargetsFromAsterisms(
    obsIds:   List[Observation.Id],
    toAdd:    List[Target.Id],
    toRemove: List[Target.Id]
  ): F[Unit] =
    val input = UpdateAsterismsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = EditAsterismsPatchInput(ADD = toAdd.assign, DELETE = toRemove.assign)
    )
    UpdateAsterismsMutation[F].execute(input).raiseGraphQLErrors.void

  def undeleteTargetsAndAddToAsterism(
    obsIds:    List[Observation.Id],
    targetIds: List[Target.Id]
  ): F[Unit] =
    val targetInput   = UpdateTargetsInput(
      WHERE = targetIds.toWhereTargets.assign,
      SET = TargetPropertiesInput(existence = Existence.Present.assign),
      includeDeleted = true.assign
    )
    val asterismInput = UpdateAsterismsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = EditAsterismsPatchInput(ADD = targetIds.assign)
    )
    UpdateTargetsAndAsterismsMutation[F].execute(targetInput, asterismInput).raiseGraphQLErrors.void

  def deleteTargetsAndRemoveFromAsterism(
    obsIds:    List[Observation.Id],
    targetIds: List[Target.Id]
  ): F[Unit] =
    val targetInput   = UpdateTargetsInput(
      WHERE = targetIds.toWhereTargets.assign,
      SET = TargetPropertiesInput(existence = Existence.Deleted.assign),
      includeDeleted = true.assign
    )
    val asterismInput = UpdateAsterismsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = EditAsterismsPatchInput(DELETE = targetIds.assign)
    )
    UpdateTargetsAndAsterismsMutation[F].execute(targetInput, asterismInput).raiseGraphQLErrors.void
