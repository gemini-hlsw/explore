// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import java.time.Instant
import java.util.concurrent.TimeUnit

import scala.concurrent.duration._
import scala.scalajs.js

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._
import clue.Backend
import clue.WebSocketReconnectionStrategy
import clue.js.AjaxJSBackend
import clue.js.WebSocketJSBackend
import crystal.AppRootContext
import crystal.react.AppRoot
import explore.common.SSOClient
import explore.model.AppConfig
import explore.model.AppContext
import explore.model.RootModel
import explore.model.UserVault
import explore.model.enum.AppTab
import explore.model.reusability._
import explore.utils.ExploreEvent
import io.chrisdavenport.log4cats.Logger
import io.circe.Json
import io.circe.syntax._
import japgolly.scalajs.react.vdom.VdomElement
import log4cats.loglevel.LogLevelLogger
import lucuma.broadcastchannel._
import lucuma.core.data.EnumZipper
import org.scalajs.dom
import org.scalajs.dom.experimental.RequestCache
import org.scalajs.dom.experimental.RequestInit
import org.scalajs.dom.experimental.{ Request => FetchRequest }
import org.scalajs.dom.ext._
import sttp.client3._
import sttp.client3.circe._
import sttp.model.Uri

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

  @JSExport
  def runIOApp(): Unit = main(Array.empty)

  override final def run(args: List[String]): IO[ExitCode] = {
    japgolly.scalajs.react.extra.ReusabilityOverlay.overrideGloballyInDev()

    def initialModel(vault: UserVault) = RootModel(
      vault = vault,
      tabs = EnumZipper.of[AppTab]
    )

    val setupScheme: IO[Unit] =
      IO.delay {
        dom.document.getElementsByTagName("body").foreach { body =>
          body.classList.remove("light-theme")
          body.classList.add("dark-theme")
        }
      }

    def repeatTokenRefresh(ssoURI: Uri, expiration: Instant, v: View[UserVault]): IO[Unit] =
      SSOClient
        .refreshToken[IO](ssoURI, expiration, v.set, IO.fromFuture)
        .attempt
        .flatMap {
          case Right(u) => repeatTokenRefresh(ssoURI, u.map(_.expiration).getOrElse(expiration), v)
          case Left(_)  => repeatTokenRefresh(ssoURI, expiration, v)
        }

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
        Uri.unsafeApply(baseURI.scheme, baseURI.host, path)
      )(port => Uri.unsafeApply(baseURI.scheme, baseURI.host, port, path))

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

    def connectParameters(vault: UserVault): IO[Map[String, Json]] =
      Map("Authorization" -> s"Bearer ${vault.token.value}".asJson).pure[IO]

    val reconnectionStrategy: WebSocketReconnectionStrategy =
      (attempt, event) =>
        if (event.code === 1000)
          none
        else // Increase the delay to get exponential backoff with a minimum of 1s and a max of 1m
          FiniteDuration(math.min(60.0, math.pow(2, attempt.toDouble - 1)).toLong,
                         TimeUnit.SECONDS
          ).some

    def setupLogoutListener(bc: BroadcastChannel[ExploreEvent]): IO[Unit] =
      IO(bc.onmessage =
        (x: ExploreEvent) =>
          // This is coming from the js world, we can't match the type
          (x.event match {
            case ExploreEvent.Logout.event =>
              IO(
                dom.window.location.reload()
              ).attempt.void
            case _                         => IO.unit
          })
      )

    for {
      _         <- setupScheme
      appConfig <- fetchConfig
      vault     <- SSOClient.vault[IO](appConfig.ssoURI, IO.fromFuture)
      _         <- logger.info(s"Git Commit: [${BuildInfo.gitHeadCommit.getOrElse("NONE")}]")
      _         <- logger.info(s"Config: ${appConfig.show}")
      ctx       <- AppContext.from[IO](appConfig, reconnectionStrategy)
      _         <- ctx.clients.odb.connect(connectParameters(vault))
      _         <- AppCtx.initIn[IO](ctx)
      _         <- setupLogoutListener(ctx.bc)
    } yield {
      val RootComponent =
        AppRoot[IO](initialModel(vault))(
          rootComponent,
          onMount = (v: View[RootModel]) =>
            repeatTokenRefresh(appConfig.ssoURI, vault.expiration, v.zoom(RootModel.vault)),
          onUnmount = (_: RootModel) => ctx.cleanup()
        )

      val container = Option(dom.document.getElementById("root")).getOrElse {
        val elem = dom.document.createElement("div")
        elem.id = "root"
        dom.document.body.appendChild(elem)
        elem
      }

      RootComponent().renderIntoDOM(container)

      ExitCode.Success
    }
  }
}
