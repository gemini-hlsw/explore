// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.target

import scala.scalajs.js
import explore.AppMain
import js.annotation._
import explore.model._
import explore._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.VdomElement

@JSExportTopLevel("TargetTest")
object Test extends AppMain {

  override def rootComponent(viewCtx: ViewCtxIO[RootModel]): VdomElement = {
    val target = viewCtx.zoomL(RootModel.target)

    <.div(^.height := "100vh", ^.width := "100%", TargetEditor(target))
  }

}
