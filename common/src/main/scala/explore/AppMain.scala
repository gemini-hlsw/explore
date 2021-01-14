// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.{ ExitCode, IO, IOApp }
import cats.syntax.all._
import clue.js.{ AjaxJSBackend, WebSocketJSBackend }
import clue.{ Backend, WebSocketReconnectionStrategy }
import crystal.AppRootContext
import crystal.react.AppRoot
import explore.model.enum.{ AppTab, ExecutionEnvironment, Theme }
import explore.model.reusability._
import explore.model.{ AppConfig, AppContext, Focused, RootModel, UserVault }
import explore.utils
import io.chrisdavenport.log4cats.Logger
import japgolly.scalajs.react.vdom.VdomElement
import log4cats.loglevel.LogLevelLogger
import lucuma.core.data.EnumZipper
import org.scalajs.dom
import org.scalajs.dom.experimental.{ Request => FetchRequest, RequestCache, RequestInit }
import org.scalajs.dom.raw.Element
import sttp.client3._
import sttp.client3.circe._
import sttp.model.Uri

import java.util.concurrent.TimeUnit
import scala.annotation.unused
import scala.concurrent.duration._
import scala.scalajs.js

import js.annotation._

object AppCtx extends AppRootContext[AppContextIO]

trait AppMain extends IOApp {
  LogLevelLogger.setLevel(LogLevelLogger.Level.INFO)

  implicit val logger: Logger[IO] = LogLevelLogger.createForRoot[IO]

  implicit val gqlHttpBackend: Backend[IO] = AjaxJSBackend[IO]

  implicit val gqlStreamingBackend: WebSocketJSBackend[IO] = WebSocketJSBackend[IO]

  protected def rootComponent(
    view: View[RootModel]
  ): VdomElement

  protected def pageUrl(@unused tab: AppTab, @unused focused: Option[Focused]): String = "#"

  @JSExport
  def runIOApp(): Unit = main(Array.empty)

  override final def run(args: List[String]): IO[ExitCode] = {
    japgolly.scalajs.react.extra.ReusabilityOverlay.overrideGloballyInDev()

    def initialModel(vault: Option[UserVault]) = RootModel(
      vault = vault,
      tabs = EnumZipper.of[AppTab]
    )

    val fetchConfig: IO[AppConfig] = {
      // We want to avoid caching the static server redirect and the config files (they are not fingerprinted by webpack).
      val backend =
        FetchBackend(customizeRequest = { request =>
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

      def httpCall =
        IO(
          basicRequest
            .get(uri)
            .readTimeout(5.seconds)
            .response(asJson[AppConfig].getRight)
            .send(backend)
        )

      IO.fromFuture(httpCall).map(_.body)
    }

    val reconnectionStrategy: WebSocketReconnectionStrategy =
      (attempt, reason) =>
        // Web Socket close codes: https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent
        if (reason.exists(_.code === 1000))
          none
        else // Increase the delay to get exponential backoff with a minimum of 1s and a max of 1m
          FiniteDuration(math.min(60.0, math.pow(2, attempt.toDouble - 1)).toLong,
                         TimeUnit.SECONDS
          ).some

    def setupDOM(env: ExecutionEnvironment): IO[Element] = IO(
      Option(dom.document.getElementById("root")).getOrElse {
        val elem = dom.document.createElement("div")
        elem.id = "root"
        dom.document.body.appendChild(elem)
        if (env === ExecutionEnvironment.Staging) {
          val stagingBanner = dom.document.createElement("div")
          stagingBanner.id = "staging-banner"
          stagingBanner.textContent = "Staging"
          dom.document.body.appendChild(stagingBanner)
        }
        elem
      }
    )

    for {
      _         <- utils.setupScheme[IO](Theme.Dark)
      appConfig <- fetchConfig
      _         <- logger.info(s"Git Commit: [${BuildInfo.gitHeadCommit.getOrElse("NONE")}]")
      _         <- logger.info(s"Config: ${appConfig.show}")
      ctx       <- AppContext.from[IO](appConfig, reconnectionStrategy, pageUrl, IO.fromFuture)
      vault     <- ctx.sso.whoami
      _         <- AppCtx.initIn[IO](ctx)
      container <- setupDOM(appConfig.environment)
    } yield {
      val RootComponent = AppRoot[IO](initialModel(vault))(rootView => rootComponent(rootView))

      RootComponent().renderIntoDOM(container)

      ExitCode.Success
    }
  }
}
