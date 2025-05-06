// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.model.GraphQLError
import clue.model.GraphQLResponse
import explore.utils.ToastCtx
import lucuma.core.model.Target
import lucuma.react.primereact.Message
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.UpdateTargetsInput
import org.typelevel.log4cats.Logger
import queries.common.TargetQueriesGQL

case class OdbApiImpl()(using FetchClient[IO, ObservationDB], Logger[IO], ToastCtx[IO])
    extends OdbApi[IO]:
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
