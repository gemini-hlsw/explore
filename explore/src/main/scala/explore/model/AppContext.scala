// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.*
import cats.effect.*
import cats.syntax.all.*
import clue.*
import clue.js.*
import clue.websocket.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.events.*
import explore.events.ExploreEvent
import explore.model.enums.AppTab
import explore.utils
import explore.utils.ToastCtx
import fs2.dom.BroadcastChannel
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.feature.Context
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.model.Program
import lucuma.react.primereact.ToastRef
import lucuma.schemas.ObservationDB
import lucuma.ui.sso.SSOClient
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import queries.schemas.SSO
import queries.schemas.UserPreferencesDB
import workers.WorkerClient

case class AppContext[F[_]](
  version:          NonEmptyString,
  clients:          GraphQLClients[F],
  workerClients:    WorkerClients[F],
  sso:              SSOClient[F],
  httpClient:       Client[F],
  pageUrl:          (AppTab, Program.Id, Focused) => String,
  setPageVia:       (AppTab, Program.Id, Focused, SetRouteVia) => Callback,
  environment:      ExecutionEnvironment,
  broadcastChannel: BroadcastChannel[F, ExploreEvent],
  toastRef:         Deferred[F, ToastRef]
)(using
  val F:            Sync[F],
  val logger:       Logger[F],
  val P:            Parallel[F]
):
  def pushPage(appTab: AppTab, programId: Program.Id, focused: Focused): Callback =
    setPageVia(appTab, programId, focused, SetRouteVia.HistoryPush)

  def replacePage(appTab: AppTab, programId: Program.Id, focused: Focused): Callback =
    setPageVia(appTab, programId, focused, SetRouteVia.HistoryReplace)

  given WebSocketJSClient[F, ObservationDB]     = clients.odb
  given WebSocketJSClient[F, UserPreferencesDB] = clients.preferencesDB
  given FetchJSClient[F, SSO]                   = clients.sso

  given itcWorker: WorkerClient[F, ItcMessage.Request]         = workerClients.itc
  given catalogWorker: WorkerClient[F, CatalogMessage.Request] = workerClients.catalog
  given agsWorker: WorkerClient[F, AgsMessage.Request]         = workerClients.ags
  given plotWorker: WorkerClient[F, PlotMessage.Request]       = workerClients.plot

  given ToastCtx[F] = new ToastCtx(toastRef)
  given Client[F]   = httpClient

  export explore.DefaultErrorPolicy

object AppContext:
  val ctx: Context[AppContext[IO]] = React.createContext("AppContext", null) // No default value

  def from[F[_]: Async: FetchJSBackend: WebSocketJSBackend: Parallel: Logger](
    config:               AppConfig,
    reconnectionStrategy: ReconnectionStrategy,
    pageUrl:              (AppTab, Program.Id, Focused) => String,
    setPageVia:           (AppTab, Program.Id, Focused, SetRouteVia) => Callback,
    workerClients:        WorkerClients[F],
    httpClient:           Client[F],
    broadcastChannel:     BroadcastChannel[F, ExploreEvent],
    toastRef:             Deferred[F, ToastRef]
  ): F[AppContext[F]] =
    for {
      clients <-
        GraphQLClients
          .build[F](
            config.odbURI,
            config.preferencesDBURI,
            config.itcURI,
            config.sso.uri,
            reconnectionStrategy
          )
      version  = utils.version(config.environment)
    } yield AppContext[F](
      version,
      clients,
      workerClients,
      SSOClient(config.sso),
      httpClient,
      pageUrl,
      setPageVia,
      config.environment,
      broadcastChannel,
      toastRef
    )

  given [F[_]]: Reusability[AppContext[F]] = Reusability.always
