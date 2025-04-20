// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.*
import cats.effect.*
import cats.syntax.all.*
import clue.js.*
import clue.websocket.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.events.*
import explore.model.enums.AppTab
import explore.utils
import explore.utils.ToastCtx
import fs2.dom.BroadcastChannel
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.feature.Context
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.model.Observation
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
  tracing:          Option[TracingConfig],
  httpClient:       Client[F],
  pageUrl:          Option[(AppTab, Program.Id, Focused)] => String,
  setPageVia:       (Option[(AppTab, Program.Id, Focused)], SetRouteVia) => Callback,
  environment:      ExecutionEnvironment,
  broadcastChannel: BroadcastChannel[F, ExploreEvent],
  toastRef:         Deferred[F, ToastRef]
)(using
  val F:            Sync[F],
  val logger:       Logger[F],
  val P:            Parallel[F]
):
  def pushPage(location: Option[(AppTab, Program.Id, Focused)]): Callback =
    setPageVia(location, SetRouteVia.HistoryPush)

  def replacePage(location: Option[(AppTab, Program.Id, Focused)]): Callback =
    setPageVia(location, SetRouteVia.HistoryReplace)

  def routingLink(
    location: Option[(AppTab, Program.Id, Focused)],
    contents: VdomNode,
    via:      SetRouteVia = SetRouteVia.HistoryPush
  ): VdomNode =
    <.a(
      ^.href := pageUrl(location),
      ^.onClick ==> (e =>
        e.preventDefaultCB >> e.stopPropagationCB >>
          setPageVia(location, via)
      )
    )(contents)

  def obsIdRoutingLink(
    programId: Program.Id,
    obsId:     Observation.Id,
    contents:  Option[VdomNode] = None
  ): VdomNode =
    val finalContents: VdomNode = contents.getOrElse(obsId.show)
    routingLink((AppTab.Observations, programId, Focused.singleObs(obsId)).some, finalContents)

  given WebSocketJsClient[F, ObservationDB]     = clients.odb
  given WebSocketJsClient[F, UserPreferencesDB] = clients.preferencesDB
  given FetchJsClient[F, SSO]                   = clients.sso

  given itcWorker: WorkerClient[F, ItcMessage.Request]         = workerClients.itc
  given catalogWorker: WorkerClient[F, CatalogMessage.Request] = workerClients.catalog
  given agsWorker: WorkerClient[F, AgsMessage.Request]         = workerClients.ags
  given plotWorker: WorkerClient[F, PlotMessage.Request]       = workerClients.plot

  given toastCtx: ToastCtx[F] = new ToastCtx(toastRef)

object AppContext:
  val ctx: Context[AppContext[IO]] = React.createContext("AppContext", null) // No default value

  def from[F[_]: Async: FetchJsBackend: WebSocketJsBackend: Parallel: Logger](
    config:               AppConfig,
    reconnectionStrategy: ReconnectionStrategy,
    pageUrl:              Option[(AppTab, Program.Id, Focused)] => String,
    setPageVia:           (Option[(AppTab, Program.Id, Focused)], SetRouteVia) => Callback,
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
            config.sso.uri,
            reconnectionStrategy
          )
      version  = utils.version(config.environment)
    } yield AppContext[F](
      version,
      clients,
      workerClients,
      SSOClient(config.sso),
      config.tracing,
      httpClient,
      pageUrl,
      setPageVia,
      config.environment,
      broadcastChannel,
      toastRef
    )

  given [F[_]]: Reusability[AppContext[F]] = Reusability.always
