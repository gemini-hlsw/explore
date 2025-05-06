// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import clue.model.GraphQLError
import clue.model.GraphQLResponse
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

case class OdbApiImpl()(using FetchClient[IO, ObservationDB], Logger[IO], ToastCtx[IO])
    extends OdbApi[IO]:

  // START TARGETS

  override def updateTarget(targetId: Target.Id, input: UpdateTargetsInput): IO[Unit] =
    // TODO REMOVE DEBUGGING LOGIC vvv
    (if (input.SET.name.exists(_.toString === "1"))
       // Logger[IO].info(s"Updating target [$targetId] with input [$input]")
       IO.raiseError(new Exception("Test Generic error"))
     else if (input.SET.name.exists(_.toString === "2"))
       IO(GraphQLResponse.errors(NonEmptyList.of(GraphQLError("Test GraphQL error")))).void
     else
       TargetQueriesGQL
         .UpdateTargetsMutation[IO]
         .execute(input)
         //  .raiseGraphQLErrorsOnNoData
         .void) // TODO ADAIWHEIHWEIWQHLIAHEDILAWHFLIAWHFLIAWHFLIWAH
      // TODO REMOVE DEBUGGING LOGIC ^^^
      .handleErrorWith(t =>
        val msg = s"Error updating target [$targetId]"
        Logger[IO].error(t)(msg) >>
          ToastCtx[IO].showToast(msg, Message.Severity.Error)
      )

  override def insertTarget(programId: Program.Id, target: Target.Sidereal): IO[Target.Id] =
    TargetQueriesGQL
      .CreateTargetMutation[IO]
      .execute(target.toCreateTargetInput(programId))
      .raiseGraphQLErrors
      .map(_.createTarget.target.id)
      .flatTap(id => ToastCtx[IO].showToast(s"Created new target [$id]"))

  override def setTargetExistence(
    programId: Program.Id,
    targetId:  Target.Id,
    existence: Existence
  ): IO[Unit] =
    TargetQueriesGQL
      .UpdateTargetsMutation[IO]
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

  override def deleteTargets(targetIds: List[Target.Id], programId: Program.Id): IO[Unit] =
    TargetQueriesGQL
      .UpdateTargetsMutation[IO]
      .execute:
        UpdateTargetsInput(
          WHERE = targetIds.toWhereTargets
            .copy(program = programId.toWhereProgram.assign)
            .assign,
          SET = TargetPropertiesInput(existence = Existence.Deleted.assign)
        )
      .raiseGraphQLErrors
      .void

  override def undeleteTargets(targetIds: List[Target.Id], programId: Program.Id): IO[Unit] =
    TargetQueriesGQL
      .UpdateTargetsMutation[IO]
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
  ): IO[TargetWithId] =
    TargetQueriesGQL
      .CloneTargetMutation[IO]
      .execute:
        CloneTargetInput(
          targetId = targetId,
          REPLACE_IN = replaceIn.toList.assign,
          SET = input.SET.assign
        )
      .raiseGraphQLErrors
      .map(_.cloneTarget.newTarget)

  // END TARGETS

  // START ASTERISMS

  // END ASTERISMS
