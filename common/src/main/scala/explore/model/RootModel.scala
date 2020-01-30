// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.graphql.client.AjaxGraphQLClient
import explore.graphql.client.WebSocketGraphQLClient
import explore.graphql.TestQuery
import cats.effect._
import crystal._
import monocle.macros.Lenses

import scala.concurrent.ExecutionContext.global

@Lenses
case class RootModel(
  target:   Option[Target]             = None,
  persons:  List[TestQuery.AllPersons] = List.empty,
  todoList: List[Task]                 = List.empty,
  polls:    List[Poll]                 = List.empty
)

object AppState {
  implicit lazy val timerIO: Timer[IO]     = cats.effect.IO.timer(global)
  implicit lazy val csIO: ContextShift[IO] = IO.contextShift(global)

  lazy val swapiClient = AjaxGraphQLClient("https://api.graph.cool/simple/v1/swapi")
  lazy val todoClient = AjaxGraphQLClient(
    "https://cors-anywhere.herokuapp.com/https://todo-mongo-graphql-server.herokuapp.com/"
  )
  lazy val pollClient = WebSocketGraphQLClient(
    // "wss://hasura-realtime-poll.herokuapp.com/v1alpha1/graphql"
    "wss://realtime-poll.demo.hasura.app/v1/graphql"
  )

  lazy val rootModel = Model[IO, RootModel](RootModel(target = Some(Target.M81)))
}
