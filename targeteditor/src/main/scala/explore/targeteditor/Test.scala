// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import scala.scalajs.js.annotation._

import cats.implicits._
import crystal.implicits._
import explore.AppMain
import explore._
import explore.model._
import gem.Observation
import gem.ProgramId
import gsp.math.Index
import japgolly.scalajs.react.vdom.html_<^._
import java.util.UUID

@JSExportTopLevel("TargetTest")
object Test extends AppMain {

  override protected def rootComponent(view: View[RootModel]): VdomElement = {
    val id = UUID.fromString("9be5789c-3ffe-48cd-8e8e-24fe3e4067ee")

    <.div(^.height := "100vh",
          ^.width := "100%",
          TargetEditor(id, /*view.zoomL(RootModel.focusedTargetOrObsId),*/ none)
    )
  }

}
