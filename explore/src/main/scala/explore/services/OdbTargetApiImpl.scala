// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.effect.Sync
import cats.effect.kernel.Resource
import cats.syntax.all.*
import clue.StreamingClient
import clue.data.syntax.*
import clue.model.GraphQLResponse
import clue.model.GraphQLResponse.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ObsIdSet
import explore.targets.TargetSearchResult
import explore.utils.ToastCtx
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.ObservationDB.Types.CloneTargetInput
import lucuma.schemas.ObservationDB.Types.TargetPropertiesInput
import lucuma.schemas.ObservationDB.Types.UpdateTargetsInput
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.odb.input.*
import queries.common.ProgramSummaryQueriesGQL.AllProgramTargets
import queries.common.TargetQueriesGQL.*

trait OdbTargetApiImpl[F[_]: Sync](using
  StreamingClient[F, ObservationDB],
  ToastCtx[F]
) extends OdbTargetApi[F]:
  self: OdbApiHelper[F] =>

  def insertTarget(programId: Program.Id, target: Target.Sidereal): F[Target.Id] =
    CreateTargetMutation[F]
      .execute(target.toCreateTargetInput(programId))
      .processErrors
      .map(_.createTarget.target.id)
      .flatTap(id => ToastCtx[F].showToast(s"Created new target [$id]"))

  def updateTarget(targetId: Target.Id, input: UpdateTargetsInput): F[Unit] =
    UpdateTargetsMutation[F]
      .execute(input)
      .processErrors
      .void

  def setTargetExistence(
    programId: Program.Id,
    targetId:  Target.Id,
    existence: Existence
  ): F[Unit] =
    UpdateTargetsMutation[F]
      .execute:
        UpdateTargetsInput(
          WHERE = targetId.toWhereTarget
            .copy(program = programId.toWhereProgram.assign)
            .assign,
          SET = TargetPropertiesInput(existence = existence.assign),
          includeDeleted = true.assign
        )
      .processErrors
      .void

  def deleteTargets(targetIds: List[Target.Id], programId: Program.Id): F[Unit] =
    UpdateTargetsMutation[F]
      .execute:
        UpdateTargetsInput(
          WHERE = targetIds.toWhereTargets
            .copy(program = programId.toWhereProgram.assign)
            .assign,
          SET = TargetPropertiesInput(existence = Existence.Deleted.assign)
        )
      .processErrors
      .void

  def undeleteTargets(targetIds: List[Target.Id], programId: Program.Id): F[Unit] =
    UpdateTargetsMutation[F]
      .execute:
        UpdateTargetsInput(
          WHERE = targetIds.toWhereTargets
            .copy(program = programId.toWhereProgram.assign)
            .assign,
          SET = TargetPropertiesInput(existence = Existence.Present.assign),
          includeDeleted = true.assign
        )
      .processErrors
      .void

  def cloneTarget(
    targetId:  Target.Id,
    replaceIn: ObsIdSet,
    input:     UpdateTargetsInput
  ): F[TargetWithId] =
    CloneTargetMutation[F]
      .execute:
        CloneTargetInput(
          targetId = targetId,
          REPLACE_IN = replaceIn.toList.assign,
          SET = input.SET.assign
        )
      .raiseGraphQLErrors
      .map(_.cloneTarget.newTarget)

  def targetEditSubscription(
    targetId: Target.Id
  ): Resource[F, fs2.Stream[F, Unit]] =
    TargetEditSubscription
      .subscribe[F](targetId)
      .raiseFirstNoDataError
      .ignoreGraphQLErrors
      .map(_.void)

  def programTargetsDeltaSubscription(
    programId: Program.Id
  ): Resource[F, fs2.Stream[F, ProgramTargetsDelta.Data.TargetEdit]] =
    ProgramTargetsDelta
      .subscribe[F](programId.toTargetEditInput)
      .processErrors("ProgramTargetsDelta")
      .map(_.map(_.targetEdit))

  def searchTargetsByNamePrefix(
    programId: Program.Id,
    name:      NonEmptyString
  ): F[List[TargetSearchResult]] =
    TargetNameQuery[F]
      .query(programId)
      .processErrors
      .map: data =>
        data.targetGroup.matches
          .map(mtch => TargetSearchResult(mtch.target.toOptId, none))
          // TODO Remove the filter when the API has a name pattern query
          .filter(_.target.name.value.toLowerCase.startsWith(name.value.toLowerCase))
          .distinct

  def allProgramTargets(programId: Program.Id): F[List[TargetWithId]] =
    drain[TargetWithId, Target.Id, AllProgramTargets.Data.Targets](
      offset =>
        AllProgramTargets[F]
          .query(programId.toWhereTarget, offset.orUnassign)
          .processErrors
          .map(_.targets),
      _.matches,
      _.hasMore,
      _.id
    )
