// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.implicits._
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import org.scalajs.dom
import scala.scalajs.js
import js.annotation._
import crystal.react.AppRoot
import io.chrisdavenport.log4cats.Logger
import clue.Backend
import clue.StreamingBackend
import clue.js.AjaxJSBackend
import clue.js.WebSocketJSBackend
import io.chrisdavenport.log4cats.log4s.Log4sLogger
import explore.model.RootModel
import explore.model.ExploreSiderealTarget
import explore.model.AppContext
import explore.model.AppConfig
import japgolly.scalajs.react.vdom.VdomElement
import org.log4s._

trait AppMain extends IOApp {

  def rootComponent(
    viewCtx: ViewCtxIO[RootModel]
  ): VdomElement

  @JSExport
  def runIOApp(): Unit = main(Array.empty)

  override final def run(args: List[String]): IO[ExitCode] = {
    Log4sConfig.setLoggerThreshold("", Info)
    // Log4sConfig.setLoggerThreshold("", AllThreshold)

    implicit val logger: Logger[IO] = Log4sLogger.createLocal[IO]

    implicit val gqlHttpBackend: Backend[IO] = AjaxJSBackend[IO]

    implicit val gqlStreamingBackend: StreamingBackend[IO] = WebSocketJSBackend[IO]

    val initialModel = RootModel(target = ExploreSiderealTarget("M81", none).some)

    AppContext.from[IO](AppConfig()).map { implicit ctx =>
      val RootComponent = AppRoot[IO](initialModel, ctx)(rootComponent, ctx.cleanup.some)

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
