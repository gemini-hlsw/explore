// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.concurrent.duration._
import scala.scalajs.js

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._
import clue.Backend
import clue.StreamingBackend
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
import io.chrisdavenport.log4cats.Logger
import japgolly.scalajs.react.vdom.VdomElement
import log4cats.loglevel.LogLevelLogger
import lucuma.core.data.EnumZipper
import org.scalajs.dom
import org.scalajs.dom.ext._
import sttp.model.Uri

import js.annotation._
import java.time.Instant
import java.time.temporal.ChronoUnit

object AppCtx extends AppRootContext[AppContextIO]

trait AppMain extends IOApp {
  LogLevelLogger.setLevel(LogLevelLogger.Level.INFO)

  implicit val logger: Logger[IO] = LogLevelLogger.createForRoot[IO]

  implicit val gqlHttpBackend: Backend[IO] = AjaxJSBackend[IO]

  implicit val gqlStreamingBackend: StreamingBackend[IO] = WebSocketJSBackend[IO]

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

    def setupScheme: IO[Unit] =
      IO.delay {
        dom.document.getElementsByTagName("body").foreach { body =>
          body.classList.remove("light-theme")
          body.classList.add("dark-theme")
        }
      }

    def refreshToken(expiration:       Instant, v: View[RootModel]): IO[UserVault] =
      IO(Instant.now).flatMap { n =>
        val sleepTime = SSOClient.refreshTimoutDelta.max(
          (n.until(expiration, ChronoUnit.SECONDS).seconds - SSOClient.refreshTimoutDelta)
        )
        IO.timer.sleep(sleepTime)
      } *> SSOClient
        .whoami[IO](IO.fromFuture)
        .flatTap((u: UserVault) => v.zoom(RootModel.vault).set(u))

    def repeatTokenRefresh(expiration: Instant, v: View[RootModel]): IO[Unit]      =
      refreshToken(expiration, v).flatMap(u => repeatTokenRefresh(u.expiration, v))

    for {
      vault        <- SSOClient.whoami[IO](IO.fromFuture)
      _            <- logger.info(s"Git Commit: [${BuildInfo.gitHeadCommit.getOrElse("NONE")}]")
      exploreDBURI <-
        IO.fromEither(Uri.parse(BuildInfo.ExploreDBEndpoint).leftMap(new Exception(_)))
      odbURI       <-
        IO.fromEither(Uri.parse(BuildInfo.ODBEndpoint).leftMap(new Exception(_)))
      ctx          <- AppContext.from[IO](AppConfig(exploreDBURI, odbURI))
      _            <- AppCtx.initIn[IO](ctx)
      _            <- setupScheme
    } yield {
      val RootComponent =
        AppRoot[IO](initialModel(vault))(
          rootComponent,
          onMount = repeatTokenRefresh(vault.expiration, _),
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
