// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.effect._
import cats.syntax.all._
import clue._
import clue.js.FetchJSBackend
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
import japgolly.scalajs.react.util.Effect
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.schemas._
import org.http4s._
import org.http4s.dom.FetchClientBuilder
import org.http4s.implicits._
import org.typelevel.log4cats.Logger
import retry._

import scala.concurrent.duration._

case class Clients[F[_]: Async: Parallel: Effect.Dispatch: Logger] protected (
  odb:           WebSocketClient[F, ObservationDB],
  preferencesDB: WebSocketClient[F, UserPreferencesDB],
  itc:           TransactionalClient[F, ITC]
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
object Clients {
  def build[F[_]: Async: TransactionalBackend: WebSocketBackend: Parallel: Effect.Dispatch: Logger](
    odbURI:               Uri,
    prefsURI:             Uri,
    itcURI:               Uri,
    reconnectionStrategy: WebSocketReconnectionStrategy
  ): F[Clients[F]] =
    for {
      odbClient   <-
        ApolloWebSocketClient.of[F, ObservationDB](odbURI, "ODB", reconnectionStrategy)
      prefsClient <-
        ApolloWebSocketClient.of[F, UserPreferencesDB](prefsURI, "PREFS", reconnectionStrategy)
      itcClient   <-
        TransactionalClient.of[F, ITC](itcURI, "ITC")
    } yield Clients(odbClient, prefsClient, itcClient)
}

case class StaticData protected (spectroscopyMatrix: SpectroscopyModesMatrix)
object StaticData {
  def build[F[_]: Async: Logger](spectroscopyMatrixUri: Uri): F[StaticData] = {
    val client = FetchClientBuilder[F]
      .withRequestTimeout(5.seconds)
      .create

    val spectroscopyMatrix =
      retryingOnAllErrors(retryPolicy[F], logError[F]("Spectroscopy Matrix")) {
        client.run(Request(Method.GET, spectroscopyMatrixUri)).use {
          case Status.Successful(r) =>
            SpectroscopyModesMatrix[F](r.bodyText)
          case fail                 =>
            // If fetching fails, do we want to continue without the matrix, or do we want to crash?
            Logger[F].warn(
              s"Could not retrieve spectroscopy matrix. Code [${fail.status.code}] - Body: [${fail.as[String]}]"
            ) >> SpectroscopyModesMatrix.empty.pure[F]
        }
      }
    spectroscopyMatrix.map(matrix => StaticData(matrix))
  }
}

case class Actions[F[_]](
  // interpreters go here
)

case class AppContext[F[_]](
  version:     NonEmptyString,
  clients:     Clients[F],
  staticData:  StaticData,
  actions:     Actions[F],
  sso:         SSOClient[F],
  pageUrl:     (AppTab, Option[Observation.Id], Option[Target.Id]) => String,
  setPage:     (AppTab, Option[Observation.Id], Option[Target.Id]) => Callback,
  environment: ExecutionEnvironment
)(implicit
  val F:       Applicative[F],
  val logger:  Logger[F],
  val P:       Parallel[F]
)

object AppContext {
  def from[F[_]: Async: FetchJSBackend: WebSocketBackend: Parallel: Effect.Dispatch: Logger](
    config:               AppConfig,
    reconnectionStrategy: WebSocketReconnectionStrategy,
    pageUrl:              (AppTab, Option[Observation.Id], Option[Target.Id]) => String,
    setPage:              (AppTab, Option[Observation.Id], Option[Target.Id]) => Callback
  ): F[AppContext[F]] =
    for {
      clients    <-
        Clients
          .build[F](config.odbURI, config.preferencesDBURI, config.itcURI, reconnectionStrategy)
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
                          config.environment
    )
}
