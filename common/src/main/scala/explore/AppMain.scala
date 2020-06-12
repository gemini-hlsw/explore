// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.scalajs.js

import cats.Id
import cats.data.NonEmptyList
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect._
import cats.implicits._
import clue.Backend
import clue.StreamingBackend
import clue.js.AjaxJSBackend
import clue.js.WebSocketJSBackend
import crystal.AppRootContext
import crystal.react.AppRoot
import explore.model.AppConfig
import explore.model.AppContext
import explore.model.ExploreSiderealTarget
import explore.model.RootModel
import explore.model.SideButton
import gpp.util.Zipper
import io.chrisdavenport.log4cats.Logger
import japgolly.scalajs.react.extra.ReusabilityOverlay
import japgolly.scalajs.react.vdom.VdomElement
import log4cats.loglevel.LogLevelLogger
import org.scalactic.anyvals.NonEmptyMap
import org.scalajs.dom

import js.annotation._

object AppCtx extends AppRootContext[AppContextIO]

trait AppMain extends IOApp {

  LogLevelLogger.setLevel(LogLevelLogger.Level.INFO)
  implicit val logger: Logger[IO] = LogLevelLogger.createForRoot[IO]

  implicit val gqlHttpBackend: Backend[IO] = AjaxJSBackend[IO]

  implicit val gqlStreamingBackend: StreamingBackend[IO] = WebSocketJSBackend[IO]

  def rootComponent(
    view: View[RootModel]
  ): VdomElement

  @JSExport
  def runIOApp(): Unit = main(Array.empty)

  override final def run(args: List[String]): IO[ExitCode] = {
    ReusabilityOverlay.overrideGloballyInDev()

    val initialModel = RootModel(tabs =
      Zipper.fromNel(
        NonEmptyList.of(SideButton("Overview"),
                        SideButton("Observations"),
                        SideButton("Target"),
                        SideButton("Configurations"),
                        SideButton("Constraints")
        )
      )
    )

    for {
      ctx <- AppContext.from[IO](AppConfig())
      _   <- AppCtx.initIn[IO](ctx)
    } yield {
      val RootComponent = AppRoot[IO](initialModel)(rootComponent, ctx.cleanup.some)

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
