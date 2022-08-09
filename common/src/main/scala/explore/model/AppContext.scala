// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.effect._
import cats.syntax.all._
import clue._
import clue.js.FetchJSBackend
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.SSOClient
import explore.events.*
import explore.model.enums.AppTab
import explore.model.enums.ExecutionEnvironment
import explore.utils
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.router.SetRouteVia
import lucuma.core.model.Program
import org.typelevel.log4cats.Logger
import workers.WebWorkerF
import workers.WorkerClient

case class AppContext[F[_]](
  version:       NonEmptyString,
  clients:       GraphQLClients[F],
  workerClients: WorkerClients[F],
  sso:           SSOClient[F],
  pageUrl:       (AppTab, Program.Id, Focused) => String,
  setPageVia:    (AppTab, Program.Id, Focused, SetRouteVia) => Callback,
  environment:   ExecutionEnvironment
)(implicit
  val F:         Applicative[F],
  val logger:    Logger[F],
  val P:         Parallel[F]
) {
  def pushPage(appTab: AppTab, programId: Program.Id, focused: Focused): Callback =
    setPageVia(appTab, programId, focused, SetRouteVia.HistoryPush)

  def replacePage(appTab: AppTab, programId: Program.Id, focused: Focused): Callback =
    setPageVia(appTab, programId, focused, SetRouteVia.HistoryReplace)
}

object AppContext {
  def from[F[_]: Async: FetchJSBackend: WebSocketBackend: Parallel: Logger](
    config:               AppConfig,
    reconnectionStrategy: WebSocketReconnectionStrategy,
    pageUrl:              (AppTab, Program.Id, Focused) => String,
    setPageVia:           (AppTab, Program.Id, Focused, SetRouteVia) => Callback,
    workerClients:        WorkerClients[F]
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
      config.environment
    )
}
