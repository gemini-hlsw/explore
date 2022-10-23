// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.*
import cats.effect.*
import cats.syntax.all.*
import clue.*
import clue.js.FetchJSBackend
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.SSOClient
import explore.events.ExploreEvent
import explore.events.*
import explore.model.enums.AppTab
import explore.model.enums.ExecutionEnvironment
import explore.utils
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.feature.Context
import lucuma.broadcastchannel.*
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import org.typelevel.log4cats.Logger
import queries.schemas.ITC
import queries.schemas.UserPreferencesDB
import workers.WebWorkerF
import workers.WorkerClient

case class AppContext[F[_]](
  version:          NonEmptyString,
  clients:          GraphQLClients[F],
  workerClients:    WorkerClients[F],
  sso:              SSOClient[F],
  pageUrl:          (AppTab, Program.Id, Focused) => String,
  setPageVia:       (AppTab, Program.Id, Focused, SetRouteVia) => Callback,
  environment:      ExecutionEnvironment,
  exploreClipboard: Ref[F, LocalClipboard],
  broadcastChannel: BroadcastChannel[ExploreEvent]
)(using
  val F:            Applicative[F],
  val logger:       Logger[F],
  val P:            Parallel[F]
):
  def pushPage(appTab: AppTab, programId: Program.Id, focused: Focused): Callback =
    setPageVia(appTab, programId, focused, SetRouteVia.HistoryPush)

  def replacePage(appTab: AppTab, programId: Program.Id, focused: Focused): Callback =
    setPageVia(appTab, programId, focused, SetRouteVia.HistoryReplace)

  given WebSocketClient[F, ObservationDB]     = clients.odb
  given WebSocketClient[F, UserPreferencesDB] = clients.preferencesDB
  given TransactionalClient[F, ITC]           = clients.itc

  given itcWorker: WorkerClient[F, ItcMessage.Request]         = workerClients.itc
  given catalogWorker: WorkerClient[F, CatalogMessage.Request] = workerClients.catalog
  given agsWorker: WorkerClient[F, AgsMessage.Request]         = workerClients.ags
  given plotWorker: WorkerClient[F, PlotMessage.Request]       = workerClients.plot

object AppContext:
  val ctx: Context[AppContext[IO]] = React.createContext(null) // No default value

  def from[F[_]: Async: FetchJSBackend: WebSocketBackend: Parallel: Logger](
    config:               AppConfig,
    reconnectionStrategy: WebSocketReconnectionStrategy,
    pageUrl:              (AppTab, Program.Id, Focused) => String,
    setPageVia:           (AppTab, Program.Id, Focused, SetRouteVia) => Callback,
    workerClients:        WorkerClients[F],
    exploreClipboard:     Ref[F, LocalClipboard],
    broadcastChannel:     BroadcastChannel[ExploreEvent]
  ): F[AppContext[F]] =
    for {
      clients <-
        GraphQLClients
          .build[F](config.odbURI, config.preferencesDBURI, config.itcURI, reconnectionStrategy)
      version  = utils.version(config.environment)
    } yield AppContext[F](
      version,
      clients,
      workerClients,
      SSOClient(config.sso),
      pageUrl,
      setPageVia,
      config.environment,
      exploreClipboard,
      broadcastChannel
    )

  given [F[_]]: Reusability[AppContext[F]] = Reusability.always
