// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.Async
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.kernel.Deferred
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import clue.WebSocketReconnectionStrategy
import clue.js.FetchJSBackend
import clue.js.FetchMethod
import clue.js.WebSocketJSBackend
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import eu.timepit.refined.collection.NonEmpty
import explore.*
import explore.components.ui.ExploreStyles
import explore.events.ExploreEvent
import explore.events.*
import explore.model.AppConfig
import explore.model.AppContext
import explore.model.ExploreLocalPreferences
import explore.model.Focused
import explore.model.Help
import explore.model.LocalClipboard
import explore.model.RootModel
import explore.model.RoutingInfo
import explore.model.UserVault
import explore.model.WorkerClients
import explore.model.enums.AppTab
import explore.model.enums.ExecutionEnvironment
import explore.model.reusability.given
import explore.syntax.ui.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.*
import japgolly.scalajs.react.vdom.html_<^.*
import log4cats.loglevel.LogLevelLogger
import lucuma.broadcastchannel.*
import lucuma.core.model.Program
import lucuma.refined.*
import lucuma.ui.enums.Theme
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.http4s.circe.*
import org.http4s.dom.FetchClientBuilder
import org.scalajs.dom
import org.scalajs.dom.Element
import org.scalajs.dom.RequestCache
import org.typelevel.log4cats.Logger
import react.primereact.ToastRef

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.scalajs.js

import js.annotation.*

@JSExportTopLevel("Explore")
object ExploreMain {

  @JSExport
  def runIOApp(): Unit = run.unsafeRunAndForget()

  def setupLogger[F[_]: Sync](p: ExploreLocalPreferences): F[Logger[F]] = Sync[F].delay {
    LogLevelLogger.setLevel(p.level)
    LogLevelLogger.createForRoot[F]
  }

  def fetchConfig[F[_]: Async]: F[AppConfig] =
    // We want to avoid caching the static server redirect and the config files (they are not fingerprinted by vite).
    AppConfig.fetchConfig(
      FetchClientBuilder[F]
        .withRequestTimeout(5.seconds)
        .withCache(RequestCache.`no-store`)
        .create
    )

  def initialModel(vault: Option[UserVault], pref: ExploreLocalPreferences) =
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
      val stagingBanner = dom.document.createElement("div")
      stagingBanner.id = "staging-banner"
      stagingBanner.textContent = "Staging"
      dom.document.body.appendChild(stagingBanner)
    }
    .whenA(env === ExecutionEnvironment.Staging)

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
    react.highcharts.Highcharts
    react.highcharts.HighchartsAccesibility
  }

  def broadcastChannel[F[_]: Sync]: Resource[F, BroadcastChannel[ExploreEvent]] =
    Resource.make(Sync[F].delay(new BroadcastChannel[ExploreEvent]("explore")))(c =>
      Sync[F].delay(c.close())
    )

  def run: IO[Unit] = {

    def setupReusabilityOverlay(env: ExecutionEnvironment): IO[Unit] =
      if (env === ExecutionEnvironment.Development) {
        toggleReusabilityOverlay[IO]() >>
          IO(japgolly.scalajs.react.extra.ReusabilityOverlay.overrideGloballyInDev())
      } else IO.unit

    val reconnectionStrategy: WebSocketReconnectionStrategy =
      (attempt, reason) =>
        // Web Socket close codes: https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent
        if (reason.toOption.flatMap(_.toOption.flatMap(_.code)).exists(_ === 1000))
          none
        else // Increase the delay to get exponential backoff with a minimum of 1s and a max of 1m
          FiniteDuration(
            math.min(60.0, math.pow(2, attempt.toDouble - 1)).toLong,
            TimeUnit.SECONDS
          ).some

    def buildPage(
      dispatcher:       Dispatcher[IO],
      workerClients:    WorkerClients[IO],
      localPreferences: ExploreLocalPreferences,
      bc:               BroadcastChannel[ExploreEvent]
    )(using Logger[IO]): IO[Unit] = {
      given FetchJSBackend[IO]     = FetchJSBackend[IO](FetchMethod.GET)
      given WebSocketJSBackend[IO] = WebSocketJSBackend[IO](dispatcher)

      val (router, routerCtl) =
        RouterWithProps.componentAndCtl(BaseUrl.fromWindowOrigin, Routing.config)

      def pageUrl(tab: AppTab, programId: Program.Id, focused: model.Focused): String =
        routerCtl.urlFor(RoutingInfo.getPage(tab, programId, focused)).value

      def setPageVia(
        tab:       AppTab,
        programId: Program.Id,
        focused:   Focused,
        via:       SetRouteVia
      ) =
        routerCtl.set(RoutingInfo.getPage(tab, programId, focused), via)

      for {
        _                    <- Theme.init[IO]
        appConfig            <- fetchConfig[IO]
        _                    <- Logger[IO].info(s"Git Commit: [${utils.gitHash.getOrElse("NONE")}]")
        _                    <- Logger[IO].info(s"Config: ${appConfig.show}")
        clipboard            <- Ref.of[IO, LocalClipboard](LocalClipboard.Empty)
        toastRef             <- Deferred[IO, ToastRef]
        ctx                  <-
          AppContext.from[IO](
            appConfig,
            reconnectionStrategy,
            pageUrl,
            setPageVia,
            workerClients,
            clipboard,
            bc,
            toastRef
          )
        _                    <- setupReusabilityOverlay(appConfig.environment)
        r                    <- (ctx.sso.whoami, setupDOM[IO], showEnvironment[IO](appConfig.environment)).parTupled
        (vault, container, _) = r
      } yield RootComponent(ctx, router, initialModel(vault, localPreferences))
        .renderIntoDOM(container)
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
      bc               <- broadcastChannel[IO]
      _                <- Resource.eval(buildPage(dispatcher, workerClients, prefs, bc))
    } yield ()).useForever
  }

}
