// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.Async
import cats.effect.IO
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.kernel.Deferred
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import clue.js.FetchJsBackend
import clue.js.FetchMethod
import clue.js.WebSocketJsBackend
import clue.websocket.ReconnectionStrategy
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.events.ExploreEvent
import explore.model.AppConfig
import explore.model.AppContext
import explore.model.ExploreLocalPreferences
import explore.model.Focused
import explore.model.RootModel
import explore.model.RoutingInfo
import explore.model.WorkerClients
import explore.model.enums.AppTab
import explore.utils.*
import fs2.dom.BroadcastChannel
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.*
import log4cats.loglevel.LogLevelLogger
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.model.Program
import lucuma.react.primereact.ToastRef
import lucuma.ui.sso.UserVault
import org.http4s.client.Client
import org.http4s.dom.FetchClientBuilder
import org.scalajs.dom
import org.scalajs.dom.Element
import org.typelevel.log4cats.Logger

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.scalajs.js

import js.annotation.*

@JSExportTopLevel("Explore", moduleID = "explore")
object ExploreMain {

  @JSExport
  def runIOApp(): Unit =
    run.unsafeRunAndForget()

  def setupLogger[F[_]: Sync](p: ExploreLocalPreferences): F[Logger[F]] = Sync[F].delay {
    LogLevelLogger.setLevel(p.level)
    LogLevelLogger.createForRoot[F]
  }

  private def buildNonCachingHttpClient[F[_]: Async]: Client[F] =
    FetchClientBuilder[F]
      .withRequestTimeout(4.seconds)
      .withCache(dom.RequestCache.`no-store`)
      .create

  def initialModel(vault: Option[UserVault], pref: ExploreLocalPreferences): RootModel =
    RootModel(vault = vault, localPreferences = pref)

  def setupDOM[F[_]: Sync]: F[Element] = Sync[F].delay(
    Option(dom.document.getElementById("root")).getOrElse {
      val elem = dom.document.createElement("div")
      elem.id = "root"
      dom.document.body.appendChild(elem)
      elem
    }
  )

  def showEnvironment[F[_]: Sync](env: ExecutionEnvironment): F[Unit] = Sync[F]
    .delay {
      val nonProdBanner = dom.document.createElement("div")
      nonProdBanner.id = "non-prod-banner"
      nonProdBanner.textContent = env.tag
      dom.document.body.appendChild(nonProdBanner)
    }
    .whenA(
      env =!= ExecutionEnvironment.Production &&
        dom.document.querySelector("#non-prod-banner") == null
    )

  def crash[F[_]: Sync](msg: String): F[Unit] =
    setupDOM[F].map { element =>
      (ExploreStyles.CrashMessage |+| ExploreStyles.ErrorLabel).value
        .foreach(element.classList.add)
      element.innerHTML = msg
    }

  // To setup the accessibility module we need to call the modules
  // This needs to be done in the right order
  def setupHighCharts[F[_]: Sync]: F[Unit] = Sync[F].delay {
    // This may seem like a no-op but it is in fact triggering the npm import
    // Order is important you need to import Highcharts first
    lucuma.react.highcharts.Highcharts
    lucuma.react.highcharts.HighchartsAccesibility
    lucuma.react.highcharts.seriesLabel.enable
  }.void

  def run: IO[Unit] = {

    def setupReusabilityOverlay(env: ExecutionEnvironment): IO[Unit] =
      if (env === ExecutionEnvironment.Development) {
        toggleReusabilityOverlay[IO]() >>
          IO(japgolly.scalajs.react.extra.ReusabilityOverlay.overrideGloballyInDev())
      } else IO.unit

    val reconnectionStrategy: ReconnectionStrategy =
      (attempt, reason) =>
        // Increase the delay to get exponential backoff with a minimum of 1s and a max of 1m
        // TODO If it's a Not authorized, do not backoff, retry on constant period.
        FiniteDuration(
          math.min(60.0, math.pow(2, attempt.toDouble - 1)).toLong,
          TimeUnit.SECONDS
        ).some

    def buildPage(
      dispatcher:       Dispatcher[IO],
      workerClients:    WorkerClients[IO],
      localPreferences: ExploreLocalPreferences,
      bc:               BroadcastChannel[IO, ExploreEvent]
    )(using Logger[IO]): IO[Unit] = {
      given FetchJsBackend[IO]     = FetchJsBackend[IO](FetchMethod.GET)
      given WebSocketJsBackend[IO] = WebSocketJsBackend[IO](dispatcher)

      val (router, routerCtl) =
        RouterWithProps.componentAndCtl(BaseUrl.fromWindowOrigin, Routing.config)

      def pageUrl(location: Option[(AppTab, Program.Id, Focused)]): String =
        routerCtl.urlFor(RoutingInfo.getPage(location)).value

      def setPageVia(
        location: Option[(AppTab, Program.Id, Focused)],
        via:      SetRouteVia
      ) =
        routerCtl.set(RoutingInfo.getPage(location), via)

      for {
        host                 <- IO(dom.window.location.host)
        httpClient            = buildNonCachingHttpClient[IO]
        appConfig            <- AppConfig.fetchConfig[IO](host, httpClient)
        _                    <- workerClients.itc.requestAndForget(ItcMessage.Initialize(appConfig.itcURI))
        _                    <- Logger[IO].info(s"Git Commit: [${utils.gitHash.getOrElse("NONE")}]")
        _                    <- Logger[IO].info(s"Config: ${appConfig.show}")
        toastRef             <- Deferred[IO, ToastRef]
        ctx                  <-
          AppContext.from[IO](
            appConfig,
            reconnectionStrategy,
            pageUrl,
            setPageVia,
            workerClients,
            httpClient,
            bc,
            toastRef
          )
        _                    <- setupReusabilityOverlay(appConfig.environment)
        r                    <- (ctx.sso.whoami, setupDOM[IO], showEnvironment[IO](appConfig.environment)).parTupled
        (vault, container, _) = r
      } yield ReactDOMClient
        .createRoot(container)
        .render:
          RootComponent(ctx, router, initialModel(vault, localPreferences))

    }.void
      .handleErrorWith { t =>
        Logger[IO].error("Error initializing") >>
          crash[IO](s"There was an error initializing Explore:<br/>${t.getMessage}")
      }

    (for {
      dispatcher       <- Dispatcher.parallel[IO]
      _                <- Resource.eval(setupHighCharts[IO])
      prefs            <- Resource.eval(ExploreLocalPreferences.loadPreferences[IO])
      given Logger[IO] <- Resource.eval(setupLogger[IO](prefs))
      workerClients    <- WorkerClients.build[IO](dispatcher)
      bc               <- BroadcastChannel[IO, ExploreEvent]("explore")
      _                <- Resource.eval(buildPage(dispatcher, workerClients, prefs, bc))
    } yield ()).useForever
  }

}
