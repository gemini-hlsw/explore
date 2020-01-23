// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.graphql.client.AjaxIOGraphQLClient
import explore.graphql.TestQuery
import cats.effect._
import crystal._
import monocle.macros.Lenses

import scala.concurrent.ExecutionContext.global

@Lenses
case class RootModel(
  target: Option[Target] = None,
  persons: List[TestQuery.AllPersons] = List.empty
)

object AppState {
  implicit private lazy val timerIO: Timer[IO]     = cats.effect.IO.timer(global)
  implicit private lazy val csIO: ContextShift[IO] = IO.contextShift(global)

  lazy val swapiClient = AjaxIOGraphQLClient("https://api.graph.cool/simple/v1/swapi")

  lazy val rootModel = Model[IO, RootModel](RootModel(target = Some(Target.M81)))
}
