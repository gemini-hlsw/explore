// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.target

import scala.scalajs.js

import explore.AppMain
import explore._
import explore.model._
import gem.Observation
import gem.ProgramId
import gsp.math.Index
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

import js.annotation._

@JSExportTopLevel("TargetTest")
object Test extends AppMain {

  override def rootComponent(viewCtx: ViewCtxIO[RootModel]): VdomElement = {
    implicit val ctx = viewCtx.ctx

    val obsId =
      Observation.Id(ProgramId.Science.fromString.getOption("GS-2020A-DS-1").get, Index.One)
    <.div(^.height := "100vh", ^.width := "100%", TargetEditor(obsId))
  }

}
