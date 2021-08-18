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
import explore.common.RetryHelpers._
import explore.common.SSOClient
import explore.model.enum.AppTab
import explore.model.enum.ExecutionEnvironment
import explore.model.reusability._
import explore.modes.SpectroscopyModesMatrix
import explore.schemas._
import explore.utils
import io.circe.Json
import japgolly.scalajs.react.Callback
import org.http4s
import org.typelevel.log4cats.Logger

import org.http4s.dom.FetchClient
import org.http4s._
import org.http4s.implicits._

case class Clients[F[_]: Async: Parallel: Dispatcher: Logger] protected (
  odb:           WebSocketClient[F, ObservationDB],
  preferencesDB: WebSocketClient[F, UserPreferencesDB]
)              {
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
object Clients {
  private def sttpUriToHttp4sUri(uri: Uri): http4s.Uri =
    http4s.Uri.unsafeFromString(uri.toString)

  def build[F[_]: Async: WebSocketBackend: Parallel: Dispatcher: Logger](
    odbURI:               Uri,
    prefsURI:             Uri,
    reconnectionStrategy: WebSocketReconnectionStrategy
  ): F[Clients[F]] =
    for {
      odbClient   <-
        ApolloWebSocketClient.of[F, ObservationDB](sttpUriToHttp4sUri(odbURI),
                                                   "ODB",
                                                   reconnectionStrategy
        )
      prefsClient <-
        ApolloWebSocketClient.of[F, UserPreferencesDB](sttpUriToHttp4sUri(prefsURI),
                                                       "PREFS",
                                                       reconnectionStrategy
        )
    } yield Clients(odbClient, prefsClient)
}

case class StaticData protected (spectroscopyMatrix: SpectroscopyModesMatrix)
object StaticData {
  def build[F[_]: Async: Logger](spectroscopyMatrixUri: Uri): F[StaticData] = {
    val spectroscopyMatrix = // FetchClient doesn't seem to support timeout yet.
      FetchClient[F].run(Request(Method.GET, spectroscopyMatrixUri)).use {
        case Status.Successful(r) =>
          SpectroscopyModesMatrix[F](r.bodyText)
        case fail                 =>
          // If fetching fails, do we want to continue without the matrix, or do we want to crash?
          Logger[F].warn(
            s"Could not retrieve spectroscopy matrix. Code [${fail.status.code}] - Body: [${fail.as[String]}]"
          ) >> SpectroscopyModesMatrix.empty.pure[F]
      }
    spectroscopyMatrix.map(matrix => StaticData(matrix))
  }
}

case class Actions[F[_]](
  // interpreters go here
)

case class AppContext[F[_]](
  version:        NonEmptyString,
  clients:        Clients[F],
  staticData:     StaticData,
  actions:        Actions[F],
  sso:            SSOClient[F],
  pageUrl:        (AppTab, Option[Focused]) => String,
  setPage:        (AppTab, Option[Focused]) => Callback,
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
      def error(t:       Throwable)(message:       => String): SyncIO[Unit] =
        f(logger.error(t)(message))
      def warn(t:        Throwable)(message:       => String): SyncIO[Unit] =
        f(logger.warn(t)(message))
      def info(t:        Throwable)(message:       => String): SyncIO[Unit] =
        f(logger.info(t)(message))
      def debug(t:       Throwable)(message:       => String): SyncIO[Unit] =
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
  def from[F[_]: Async: WebSocketBackend: Parallel: Dispatcher: Logger](
    config:               AppConfig,
    reconnectionStrategy: WebSocketReconnectionStrategy,
    pageUrl:              (AppTab, Option[Focused]) => String,
    setPage:              (AppTab, Option[Focused]) => Callback,
    fromSyncIO:           SyncIO ~> F
  ): F[AppContext[F]] =
    for {
      clients    <- Clients.build[F](config.odbURI, config.preferencesDBURI, reconnectionStrategy)
      staticData <- StaticData.build[F](uri"/instrument_spectroscopy_matrix.csv")
      version     = utils.version(config.environment)
      actions     = Actions[F]()
    } yield AppContext[F](version,
                          clients,
                          staticData,
                          actions,
                          SSOClient(config.sso),
                          pageUrl,
                          setPage,
                          config.environment,
                          fromSyncIO
    )
}
