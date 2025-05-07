// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.Applicative
import cats.MonadThrow
import cats.data.NonEmptyList
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import clue.model.GraphQLError
import clue.model.GraphQLResponse
import clue.model.GraphQLResponse.*
import explore.model.ObsIdSet
import explore.utils.ToastCtx
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.react.primereact.Message
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.ObservationDB.Types.CloneTargetInput
import lucuma.schemas.ObservationDB.Types.TargetPropertiesInput
import lucuma.schemas.ObservationDB.Types.UpdateTargetsInput
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.odb.input.*
import org.typelevel.log4cats.Logger
import queries.common.TargetQueriesGQL

trait OdbTargetApiImpl[F[_]: MonadThrow](using
  FetchClient[F, ObservationDB],
  Logger[F],
  ToastCtx[F]
) extends OdbTargetApi[F]:

  override def updateTarget(targetId: Target.Id, input: UpdateTargetsInput): F[Unit] =
    // TODO REMOVE DEBUGGING LOGIC vvv
    (if (input.SET.name.exists(_.toString === "1"))
       // Logger[IO].info(s"Updating target [$targetId] with input [$input]")
       MonadThrow[F].raiseError(new Exception("Test Generic error"))
     else if (input.SET.name.exists(_.toString === "2"))
       Applicative[F]
         .pure(GraphQLResponse.errors(NonEmptyList.of(GraphQLError("Test GraphQL error"))))
         .void
     else
       TargetQueriesGQL
         .UpdateTargetsMutation[F]
         .execute(input)
         //  .raiseGraphQLErrorsOnNoData
         .void) // TODO ADAIWHEIHWEIWQHLIAHEDILAWHFLIAWHFLIAWHFLIWAH
      // TODO REMOVE DEBUGGING LOGIC ^^^
      .handleErrorWith(t =>
        val msg = s"Error updating target [$targetId]"
        Logger[F].error(t)(msg) >>
          ToastCtx[F].showToast(msg, Message.Severity.Error)
      )

  override def insertTarget(programId: Program.Id, target: Target.Sidereal): F[Target.Id] =
    TargetQueriesGQL
      .CreateTargetMutation[F]
      .execute(target.toCreateTargetInput(programId))
      .raiseGraphQLErrors
      .map(_.createTarget.target.id)
      .flatTap(id => ToastCtx[F].showToast(s"Created new target [$id]"))

  override def setTargetExistence(
    programId: Program.Id,
    targetId:  Target.Id,
    existence: Existence
  ): F[Unit] =
    TargetQueriesGQL
      .UpdateTargetsMutation[F]
      .execute:
        UpdateTargetsInput(
          WHERE = targetId.toWhereTarget
            .copy(program = programId.toWhereProgram.assign)
            .assign,
          SET = TargetPropertiesInput(existence = existence.assign),
          includeDeleted = true.assign
        )
      .raiseGraphQLErrors
      .void

  override def deleteTargets(targetIds: List[Target.Id], programId: Program.Id): F[Unit] =
    TargetQueriesGQL
      .UpdateTargetsMutation[F]
      .execute:
        UpdateTargetsInput(
          WHERE = targetIds.toWhereTargets
            .copy(program = programId.toWhereProgram.assign)
            .assign,
          SET = TargetPropertiesInput(existence = Existence.Deleted.assign)
        )
      .raiseGraphQLErrors
      .void

  override def undeleteTargets(targetIds: List[Target.Id], programId: Program.Id): F[Unit] =
    TargetQueriesGQL
      .UpdateTargetsMutation[F]
      .execute:
        UpdateTargetsInput(
          WHERE = targetIds.toWhereTargets
            .copy(program = programId.toWhereProgram.assign)
            .assign,
          SET = TargetPropertiesInput(existence = Existence.Present.assign),
          includeDeleted = true.assign
        )
      .raiseGraphQLErrors
      .void

  override def cloneTarget(
    targetId:  Target.Id,
    replaceIn: ObsIdSet,
    input:     UpdateTargetsInput
  ): F[TargetWithId] =
    TargetQueriesGQL
      .CloneTargetMutation[F]
      .execute:
        CloneTargetInput(
          targetId = targetId,
          REPLACE_IN = replaceIn.toList.assign,
          SET = input.SET.assign
        )
      .raiseGraphQLErrors
      .map(_.cloneTarget.newTarget)
