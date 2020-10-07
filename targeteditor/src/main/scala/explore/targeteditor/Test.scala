// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import java.util.UUID

import scala.scalajs.js.annotation._

import explore.AppMain
import explore._
import explore.components.Tile
import explore.model._
import japgolly.scalajs.react.vdom.html_<^._

@JSExportTopLevel("TargetTest")
object Test extends AppMain {

  override protected def rootComponent(view: View[RootModel]): VdomElement = {
    val id = UUID.fromString("b9acf8b4-79e9-4c69-9a96-904746e127ab")

    <.div(^.height := "100vh",
          ^.width := "100%",
          Tile("Target", false)(
            TargetEditor(id)
          )
    )
  }

}
