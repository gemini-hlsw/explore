// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.*
import cats.effect.*
import cats.syntax.all.*
import clue.*
import io.circe.Json
import lucuma.schemas.*
import org.http4s.Uri.Authority
import org.http4s.Uri.Scheme
import org.http4s.*
import org.http4s.syntax.all.*
import org.typelevel.log4cats.Logger
import queries.schemas.*

case class GraphQLClients[F[_]: Async: Parallel] protected (
  odb:           WebSocketClient[F, ObservationDB],
  preferencesDB: WebSocketClient[F, UserPreferencesDB],
  itc:           TransactionalClient[F, ITC],
  sso:           WebSocketClient[F, SSO]
):
  def init(payload: Map[String, Json]): F[Unit] =
    (
      preferencesDB.connect() >> preferencesDB.initialize(),
      odb.connect() >> odb.initialize(payload),
      sso.connect() >> sso.initialize(payload)
    ).parTupled.void

  def close(): F[Unit] =
    List(
      preferencesDB.terminate() >> preferencesDB.disconnect(WebSocketCloseParams(code = 1000)),
      odb.terminate() >> odb.disconnect(WebSocketCloseParams(code = 1000)),
      sso.terminate() >> sso.disconnect(WebSocketCloseParams(code = 1000))
    ).sequence.void

object GraphQLClients:
  def build[F[_]: Async: TransactionalBackend: WebSocketBackend: Parallel: Logger](
    odbURI:               Uri,
    prefsURI:             Uri,
    itcURI:               Uri,
    ssoURI:               Uri,
    reconnectionStrategy: WebSocketReconnectionStrategy
  ): F[GraphQLClients[F]] =
    for {
      odbClient   <-
        ApolloWebSocketClient.of[F, ObservationDB](odbURI, "ODB", reconnectionStrategy)
      prefsClient <-
        ApolloWebSocketClient.of[F, UserPreferencesDB](prefsURI, "PREFS", reconnectionStrategy)
      itcClient   <-
        TransactionalClient.of[F, ITC](itcURI, "ITC")
      ssoClient   <-
        val ssoURL =
          Uri(Scheme.fromString(s"wss").toOption,
              ssoURI.host.map(h => Authority(host = h)),
              path"ws"
          )
        ApolloWebSocketClient.of[F, SSO](ssoURL, "SSO", reconnectionStrategy)
    } yield GraphQLClients(odbClient, prefsClient, itcClient, ssoClient)
