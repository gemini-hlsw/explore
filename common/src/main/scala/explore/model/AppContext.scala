// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.effect._
import cats.syntax.all._
import clue._
import crystal.react.StreamRenderer
import eu.timepit.refined.types.string.NonEmptyString
import explore.GraphQLSchemas._
import explore.common.SSOClient
import explore.model.enum.AppTab
import explore.model.reusability._
import explore.utils
import io.chrisdavenport.log4cats.Logger
import io.circe.Json
import sttp.client3.Response

case class Clients[F[_]: ConcurrentEffect: Logger](
  exploreDB: GraphQLWebSocketClient[F, ExploreDB],
  odb:       GraphQLWebSocketClient[F, ObservationDB]
) {
  lazy val ExploreDBConnectionStatus: StreamRenderer.Component[StreamingClientStatus] =
    StreamRenderer.build(exploreDB.statusStream)

  lazy val ODBConnectionStatus: StreamRenderer.Component[StreamingClientStatus] =
    StreamRenderer.build(odb.statusStream)

  def init(payload: F[Map[String, Json]]): F[Unit] =
    exploreDB.init() >> odb.init(payload)

  def disconnect(): F[Unit] =
    List(
      exploreDB.terminate(TerminateOptions.Disconnect(WebSocketCloseParams(code = 1000))),
      odb.terminate(TerminateOptions.Disconnect(WebSocketCloseParams(code = 1000)))
    ).sequence.void
}

case class Actions[F[_]](
  // interpreters go here
)

case class AppContext[F[_]](
  version:    NonEmptyString,
  clients:    Clients[F],
  actions:    Actions[F],
  sso:        SSOClient[F],
  pageUrl:    (AppTab, Option[Focused]) => String
)(implicit
  val F:      Applicative[F],
  val cs:     ContextShift[F],
  val timer:  Timer[F],
  val logger: Logger[F]
)

object AppContext {
  def from[F[_]: ConcurrentEffect: ContextShift: Timer: Logger: Backend: WebSocketBackend](
    config:               AppConfig,
    reconnectionStrategy: WebSocketReconnectionStrategy,
    pageUrl:              (AppTab, Option[Focused]) => String,
    fromFuture:           SSOClient.FromFuture[F, Response[Either[String, String]]]
  ): F[AppContext[F]] =
    for {
      exploreDBClient <-
        ApolloWebSocketClient.of[F, ExploreDB](config.exploreDBURI, reconnectionStrategy)
      odbClient       <- ApolloWebSocketClient.of[F, ObservationDB](config.odbURI, reconnectionStrategy)
      version          = utils.version(config.environment)
      clients          = Clients(exploreDBClient, odbClient)
      actions          = Actions[F]()
    } yield AppContext[F](version, clients, actions, SSOClient(config.sso, fromFuture), pageUrl)
}
