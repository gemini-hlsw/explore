// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.Async
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.std.Dispatcher
import cats.syntax.all._
import clue.WebSocketReconnectionStrategy
import clue.js.FetchJSBackend
import clue.js.FetchMethod
import clue.js.WebSocketJSBackend
import crystal.react._
import crystal.react.reuse._
import eu.timepit.refined.collection.NonEmpty
import explore._
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.model.AppConfig
import explore.model.AppContext
import explore.model.ExploreLocalPreferences
import explore.model.Focused
import explore.model.RootModel
import explore.model.RoutingInfo
import explore.model.UserVault
import explore.model.WorkerClients
import explore.model.enums.AppTab
import explore.model.enums.ExecutionEnvironment
import explore.model.enums.Theme
import explore.model.reusability._
import explore.syntax.ui.given
import explore.utils._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import log4cats.loglevel.LogLevelLogger
import lucuma.core.model.Program
import lucuma.refined.*
import lucuma.ui.syntax.all.*
import org.http4s.circe._
import org.http4s.dom.FetchClientBuilder
import org.http4s.implicits._
import org.scalajs.dom
import org.scalajs.dom.Element
import org.scalajs.dom.RequestCache
import org.typelevel.log4cats.Logger
import react.toastify._

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.scalajs.js

import js.annotation._

@JSExportTopLevel("Explore")
object ExploreMain extends IOApp.Simple {

  @JSExport
  def resetIOApp(): Unit =
    // https://github.com/typelevel/cats-effect/pull/2114#issue-687064738
    cats.effect.unsafe.JSPlatform.resetRuntime()

  @JSExport
  def runIOApp(): Unit = main(Array.empty)

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
      (ExploreStyles.CrashMessage |+| ExploreStyles.ErrorLabel).htmlClasses
        .foreach(element.classList.add)
      element.innerHTML = msg
    }

  // To setup the accessibility module we need to call the modules
  // This needs to be done in the right order
  def setupHighCharts[F[_]: Sync]: F[Unit] = Sync[F].delay {
    react.highcharts.Highcharts
    explore.highcharts.HighchartsAccesibility
  }

  override final def run: IO[Unit] = {

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
      localPreferences: ExploreLocalPreferences
    )(using Logger[IO]): IO[Unit] = {
      given FetchJSBackend[IO]     = FetchJSBackend[IO](FetchMethod.GET)
      given WebSocketJSBackend[IO] = WebSocketJSBackend[IO](dispatcher)

      val (router, routerCtl) =
        RouterWithProps.componentAndCtl(BaseUrl.fromWindowOrigin, Routing.config)

      def rootComponent(view: ReuseView[RootModel]): VdomElement =
        <.div(
          router(view),
          ToastContainer(
            position = Position.BottomRight,
            theme = react.toastify.Theme.Dark,
            clazz = ExploreStyles.ExploreToast
          )
        )

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
        _                    <- utils.setupScheme[IO](Theme.Dark)
        appConfig            <- fetchConfig[IO]
        _                    <- Logger[IO].info(s"Git Commit: [${utils.gitHash.getOrElse("NONE")}]")
        _                    <- Logger[IO].info(s"Config: ${appConfig.show}")
        ctx                  <-
          AppContext.from[IO](
            appConfig,
            reconnectionStrategy,
            pageUrl,
            setPageVia,
            workerClients
          )
        _                    <- setupReusabilityOverlay(appConfig.environment)
        r                    <- (ctx.sso.whoami, setupDOM[IO], showEnvironment[IO](appConfig.environment)).parTupled
        (vault, container, _) = r
      } yield {
        val RootComponent =
          ContextProvider(AppCtx, ctx)

        val HelpContextComponent =
          ContextProvider(
            HelpCtx,
            HelpContext(
              rawUrl = uri"https://raw.githubusercontent.com",
              editUrl = uri"https://github.com",
              user = "gemini-hlsw",
              project = "explore-help-docs",
              displayedHelp = none
            )
          )

        val StateProviderComponent =
          StateProvider(initialModel(vault, localPreferences))

        RootComponent(
          (HelpContextComponent(
            (StateProviderComponent((rootComponent _).reuseAlways): VdomNode).reuseAlways
          ): VdomNode).reuseAlways
        ).renderIntoDOM(container)
      }
    }.void
      .handleErrorWith { t =>
        IO.println("Error initializing") >>
          crash[IO](s"There was an error initializing Explore:<br/>${t.getMessage}")
      }

    (for {
      dispatcher       <- Dispatcher[IO]
      _                <- Resource.eval(setupHighCharts[IO])
      prefs            <- Resource.eval(ExploreLocalPreferences.loadPreferences[IO])
      given Logger[IO] <- Resource.eval(setupLogger[IO](prefs))
      workerClients    <- WorkerClients.build[IO](dispatcher)
      _                <- Resource.eval(buildPage(dispatcher, workerClients, prefs))
    } yield ()).useForever
  }

}
