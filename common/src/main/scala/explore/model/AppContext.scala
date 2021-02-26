// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import explore.model.enum.ExecutionEnvironment
import explore.model.reusability._
import explore.utils
import io.chrisdavenport.log4cats.Logger
import io.circe.Json
import sttp.client3.Response
import sttp.model.Uri

case class Clients[F[_]: ConcurrentEffect: Parallel: Logger](
  odb:           WebSocketClient[F, ObservationDB],
  preferencesDB: WebSocketClient[F, UserPreferencesDB]
) {
  lazy val PreferencesDBConnectionStatus: StreamRenderer.Component[PersistentClientStatus] =
    StreamRenderer.build(preferencesDB.statusStream)

  lazy val ODBConnectionStatus: StreamRenderer.Component[PersistentClientStatus] =
    StreamRenderer.build(odb.statusStream)

  def init(payload: F[Map[String, Json]]): F[Unit] =
    (
      preferencesDB.connect() >> preferencesDB.initialize(),
      odb.connect() >> odb.initialize(payload)
    ).parTupled.void

  def close(): F[Unit] =
    List(
      preferencesDB.terminate() >> preferencesDB.disconnect(WebSocketCloseParams(code = 1000)),
      odb.terminate() >> odb.disconnect(WebSocketCloseParams(code = 1000))
    ).sequence.void
}

case class Actions[F[_]](
  // interpreters go here
)

case class AppContext[F[_]](
  version:     NonEmptyString,
  clients:     Clients[F],
  actions:     Actions[F],
  sso:         SSOClient[F],
  pageUrl:     (AppTab, Option[Focused]) => String,
  environment: ExecutionEnvironment
)(implicit
  val F:       Applicative[F],
  val cs:      ContextShift[F],
  val timer:   Timer[F],
  val logger:  Logger[F]
)

object AppContext {
  private def buildClients[F[_]: ConcurrentEffect: WebSocketBackend: Parallel: Timer: Logger](
    odbURI:               Uri,
    prefsURI:             Uri,
    reconnectionStrategy: WebSocketReconnectionStrategy
  ): F[Clients[F]] =
    for {
      odbClient   <-
        ApolloWebSocketClient.of[F, ObservationDB](odbURI, "ODB", reconnectionStrategy)
      prefsClient <-
        ApolloWebSocketClient.of[F, UserPreferencesDB](prefsURI, "PREFS", reconnectionStrategy)
    } yield Clients(odbClient, prefsClient)

  def from[F[_]: ConcurrentEffect: WebSocketBackend: Parallel: ContextShift: Timer: Logger](
    config:               AppConfig,
    reconnectionStrategy: WebSocketReconnectionStrategy,
    pageUrl:              (AppTab, Option[Focused]) => String,
    fromFuture:           SSOClient.FromFuture[F, Response[Either[String, String]]]
  ): F[AppContext[F]] =
    for {
      clients <- buildClients(config.odbURI, config.preferencesDBURI, reconnectionStrategy)
      version  = utils.version(config.environment)
      actions  = Actions[F]()
    } yield AppContext[F](version,
                          clients,
                          actions,
                          SSOClient(config.sso, fromFuture),
                          pageUrl,
                          config.environment
    )
}
