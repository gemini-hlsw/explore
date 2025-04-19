// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.Async
import cats.implicits.*
import clue.FetchClient
import clue.data.syntax.*
import explore.model.Observation
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import queries.common.AsterismQueriesGQL.*
import queries.common.ObsQueriesGQL.*

object AsterismQueries:

  def replaceAsterism[F[_]: Async](
    obsIds:    List[Observation.Id],
    targetIds: List[Target.Id]
  )(using FetchClient[F, ObservationDB]) =
    val input = UpdateObservationsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = ObservationPropertiesInput(
        targetEnvironment = TargetEnvironmentInput(asterism = targetIds.assign).assign
      )
    )
    UpdateObservationMutation[F].execute(input).void

  def addTargetsToAsterisms[F[_]: Async](
    obsIds:    List[Observation.Id],
    targetIds: List[Target.Id]
  )(using FetchClient[F, ObservationDB]) =
    val input = UpdateAsterismsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = EditAsterismsPatchInput(ADD = targetIds.assign)
    )
    UpdateAsterismsMutation[F].execute(input).void

  def removeTargetsFromAsterisms[F[_]: Async](
    obsIds:    List[Observation.Id],
    targetIds: List[Target.Id]
  )(using FetchClient[F, ObservationDB]) =
    val input = UpdateAsterismsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = EditAsterismsPatchInput(DELETE = targetIds.assign)
    )
    UpdateAsterismsMutation[F].execute(input).void

  def addAndRemoveTargetsFromAsterisms[F[_]: Async](
    obsIds:   List[Observation.Id],
    toAdd:    List[Target.Id],
    toRemove: List[Target.Id]
  )(using FetchClient[F, ObservationDB]) =
    val input = UpdateAsterismsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = EditAsterismsPatchInput(ADD = toAdd.assign, DELETE = toRemove.assign)
    )
    UpdateAsterismsMutation[F].execute(input).void

  def undeleteTargetsAndAddToAsterism[F[_]: Async](
    obsIds:    List[Observation.Id],
    targetIds: List[Target.Id]
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    val targetInput   = UpdateTargetsInput(
      WHERE = targetIds.toWhereTargets.assign,
      SET = TargetPropertiesInput(existence = Existence.Present.assign),
      includeDeleted = true.assign
    )
    val asterismInput = UpdateAsterismsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = EditAsterismsPatchInput(ADD = targetIds.assign)
    )
    UpdateTargetsAndAsterismsMutation[F].execute(targetInput, asterismInput).void

  def deleteTargetsAndRemoveFromAsterism[F[_]: Async](
    obsIds:    List[Observation.Id],
    targetIds: List[Target.Id]
  )(using FetchClient[F, ObservationDB]): F[Unit] =
    val targetInput   = UpdateTargetsInput(
      WHERE = targetIds.toWhereTargets.assign,
      SET = TargetPropertiesInput(existence = Existence.Deleted.assign),
      includeDeleted = true.assign
    )
    val asterismInput = UpdateAsterismsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = EditAsterismsPatchInput(DELETE = targetIds.assign)
    )
    UpdateTargetsAndAsterismsMutation[F].execute(targetInput, asterismInput).void
