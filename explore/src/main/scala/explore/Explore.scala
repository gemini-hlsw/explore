// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.SyncIO
import cats.effect.std.Dispatcher
import cats.syntax.all._
import cats.~>
import clue.WebSocketReconnectionStrategy
import clue.js.WebSocketJSBackend
import crystal.react._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.components.ui.ExploreStyles
import explore.model.AppConfig
import explore.model.AppContext
import explore.model.Focused
import explore.model.RootModel
import explore.model.RootModelRouting
import explore.model.UserVault
import explore.model.enum.AppTab
import explore.model.enum.ExecutionEnvironment
import explore.model.enum.Theme
import explore.model.reusability._
import explore.utils
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import log4cats.loglevel.LogLevelLogger
import lucuma.core.data.EnumZipper
import org.scalajs.dom
import org.scalajs.dom.experimental.RequestCache
import org.scalajs.dom.experimental.RequestInit
import org.scalajs.dom.experimental.{ Request => FetchRequest }
import org.scalajs.dom.raw.Element
import org.typelevel.log4cats.Logger
import react.common.implicits._
import sttp.client3._
import sttp.client3.circe._
import sttp.client3.impl.cats.FetchCatsBackend
import sttp.model.Uri

import java.util.concurrent.TimeUnit
import scala.annotation.nowarn
import scala.concurrent.duration._
import scala.scalajs.js

import js.annotation._

@JSExportTopLevel("Explore")
object ExploreMain extends IOApp.Simple {

  LogLevelLogger.setLevel(LogLevelLogger.Level.INFO)

  implicit val reuseContext: Reusability[AppContextIO] = Reusability.never

  implicit val logger: Logger[IO] = LogLevelLogger.createForRoot[IO]

  val syncIOtoIO: SyncIO ~> IO = new ~>[SyncIO, IO] {
    def apply[A](fa: SyncIO[A]): IO[A] = fa.to[IO]
  }

  @JSExport
  @nowarn
  def resetIOApp(): Unit =
    // https://github.com/typelevel/cats-effect/pull/2114#issue-687064738
    cats.effect.unsafe.IORuntime.asInstanceOf[{ def resetGlobal(): Unit }].resetGlobal()

  @JSExport
  def runIOApp(): Unit = main(Array.empty)

  override final def run: IO[Unit] = {
    japgolly.scalajs.react.extra.ReusabilityOverlay.overrideGloballyInDev()

    def initialModel(vault: Option[UserVault]) = RootModel(
      vault = vault,
      tabs = EnumZipper.of[AppTab]
    )

    val fetchConfig: IO[AppConfig] = {
      // We want to avoid caching the static server redirect and the config files (they are not fingerprinted by webpack).
      val backend =
        FetchCatsBackend[IO](customizeRequest = { request =>
          new FetchRequest(request,
                           new RequestInit() {
                             cache = RequestCache.`no-store`
                           }
          )
        })

      // No relative URIs yet in STTP: https://github.com/softwaremill/sttp/issues/285
      // val uri = uri"/conf.json"
      val baseURI = Uri.unsafeParse(dom.window.location.href)
      val path    = List("conf.json")
      val uri     = baseURI.port.fold(
        Uri.unsafeApply(baseURI.scheme.orEmpty, baseURI.host.orEmpty, path)
      )(port => Uri.unsafeApply(baseURI.scheme.orEmpty, baseURI.host.orEmpty, port, path))

      basicRequest
        .get(uri)
        .readTimeout(5.seconds)
        .response(asJson[AppConfig].getRight)
        .send(backend)
        .map(_.body)
    }.adaptError { case t =>
      new Exception("Could not retrieve configuration.", t)
    }

    val reconnectionStrategy: WebSocketReconnectionStrategy =
      (attempt, reason) =>
        // Web Socket close codes: https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent
        if (reason.toOption.flatMap(_.toOption.flatMap(_.code)).exists(_ === 1000))
          none
        else // Increase the delay to get exponential backoff with a minimum of 1s and a max of 1m
          FiniteDuration(math.min(60.0, math.pow(2, attempt.toDouble - 1)).toLong,
                         TimeUnit.SECONDS
          ).some

    def setupDOM(): IO[Element] = IO(
      Option(dom.document.getElementById("root")).getOrElse {
        val elem = dom.document.createElement("div")
        elem.id = "root"
        dom.document.body.appendChild(elem)
        elem
      }
    )

    def showEnvironment(env: ExecutionEnvironment): IO[Unit] = IO {
      val stagingBanner = dom.document.createElement("div")
      stagingBanner.id = "staging-banner"
      stagingBanner.textContent = "Staging"
      dom.document.body.appendChild(stagingBanner)
    }.whenA(env === ExecutionEnvironment.Staging)

    def crash(msg: String): IO[Unit] =
      setupDOM().map { element =>
        (ExploreStyles.CrashMessage |+| ExploreStyles.ErrorLabel).htmlClasses
          .foreach(element.classList.add)
        element.innerHTML = msg
      }

    Dispatcher[IO].allocated
      .map(_._1)
      .flatMap { implicit dispatcher =>
        implicit val gqlStreamingBackend: WebSocketJSBackend[IO] =
          WebSocketJSBackend[IO](dispatcher)

        val (router, routerCtl) =
          RouterWithProps.componentAndCtl(BaseUrl.fromWindowOrigin, Routing.config)

        def routingView(view: View[RootModel]): View[RootModel] =
          view.withOnMod { model =>
            routerCtl
              .set(RootModelRouting.lens.get(model))
              .to[SyncIO]
          }

        def rootComponent(view: View[RootModel]): VdomElement =
          <.div(
            router(routingView(view))
          )

        def pageUrl(tab: AppTab, focused: Option[Focused]): String =
          routerCtl.urlFor(RootModelRouting.getPage(tab, focused)).value

        def setPage(tab: AppTab, focused: Option[Focused]) =
          routerCtl.set(RootModelRouting.getPage(tab, focused))

        for {
          _                    <- utils.setupScheme[IO](Theme.Dark)
          appConfig            <- fetchConfig
          _                    <- logger.info(s"Git Commit: [${utils.gitHash.getOrElse("NONE")}]")
          _                    <- logger.info(s"Config: ${appConfig.show}")
          ctx                  <- AppContext.from[IO](appConfig, reconnectionStrategy, pageUrl, setPage, syncIOtoIO)
          r                    <- (ctx.sso.whoami, setupDOM(), showEnvironment(appConfig.environment)).parTupled
          (vault, container, _) = r
        } yield {
          val RootComponent =
            ContextProviderSyncIO(AppCtx, ctx)

          val HelpContextComponent =
            ContextProviderSyncIO(
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
            StateProviderSyncIO(initialModel(vault))

          RootComponent(
            (HelpContextComponent(
              (StateProviderComponent((rootComponent _).reuseAlways): VdomNode).reuseAlways
            ): VdomNode).reuseAlways
          ).renderIntoDOM(container)
        }
      }
      .void
      .handleErrorWith { t =>
        logger.error(t)("Error initializing") >>
          crash(s"There was an error initializing Explore:<br/>${t.getMessage}")
      }
  }

}
