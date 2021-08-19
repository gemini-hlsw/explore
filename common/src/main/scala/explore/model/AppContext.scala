// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.effect._
import cats.effect.std.Dispatcher
import cats.syntax.all._
import clue._
import crystal.react.StreamRenderer
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.SSOClient
import explore.model.enum.AppTab
import explore.model.enum.ExecutionEnvironment
import explore.model.reusability._
import explore.schemas._
import explore.utils
import io.circe.Json
import org.typelevel.log4cats.Logger
import sttp.model.Uri

case class Clients[F[_]: Async: Parallel: Dispatcher: Logger](
  odb:           WebSocketClient[F, ObservationDB],
  preferencesDB: WebSocketClient[F, UserPreferencesDB]
) {
  lazy val PreferencesDBConnectionStatus: StreamRenderer.Component[PersistentClientStatus] =
    StreamRenderer.build(preferencesDB.statusStream)

  lazy val ODBConnectionStatus: StreamRenderer.Component[PersistentClientStatus] =
    StreamRenderer.build(odb.statusStream)

  def init(payload: Map[String, Json]): F[Unit] =
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
  version:        NonEmptyString,
  clients:        Clients[F],
  actions:        Actions[F],
  sso:            SSOClient[F],
  pageUrl:        (AppTab, Option[Focused]) => String,
  environment:    ExecutionEnvironment,
  fromSyncIO:     SyncIO ~> F
)(implicit
  val F:          Applicative[F],
  val dispatcher: Dispatcher[F],
  val logger:     Logger[F]
) {
  val syncLogger: Logger[SyncIO] = {
    def f(x: F[Unit]): SyncIO[Unit] = SyncIO(dispatcher.unsafeRunAndForget(x))
    new Logger[SyncIO] {
      def error(t:       Throwable)(message: => String): SyncIO[Unit] =
        f(logger.error(t)(message))
      def warn(t:        Throwable)(message: => String): SyncIO[Unit] =
        f(logger.warn(t)(message))
      def info(t:        Throwable)(message: => String): SyncIO[Unit] =
        f(logger.info(t)(message))
      def debug(t:       Throwable)(message: => String): SyncIO[Unit] =
        f(logger.debug(t)(message))
      def trace(t:       Throwable)(message: => String): SyncIO[Unit] =
        f(logger.trace(t)(message))
      def error(message: => String): SyncIO[Unit] =
        f(logger.error(message))
      def warn(message:  => String): SyncIO[Unit] =
        f(logger.warn(message))
      def info(message:  => String): SyncIO[Unit] =
        f(logger.info(message))
      def debug(message: => String): SyncIO[Unit] =
        f(logger.debug(message))
      def trace(message: => String): SyncIO[Unit] =
        f(logger.trace(message))
    }
  }

}

object AppContext {
  private def buildClients[F[_]: Async: WebSocketBackend: Parallel: Dispatcher: Logger](
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

  def from[F[_]: Async: WebSocketBackend: Parallel: Dispatcher: Logger](
    config:               AppConfig,
    reconnectionStrategy: WebSocketReconnectionStrategy,
    pageUrl:              (AppTab, Option[Focused]) => String,
    fromSyncIO:           SyncIO ~> F
  ): F[AppContext[F]] =
    for {
      clients <- buildClients(config.odbURI, config.preferencesDBURI, reconnectionStrategy)
      version  = utils.version(config.environment)
      actions  = Actions[F]()
    } yield AppContext[F](version,
                          clients,
                          actions,
                          SSOClient(config.sso),
                          pageUrl,
                          config.environment,
                          fromSyncIO
    )
}
