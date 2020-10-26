// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import scala.scalajs.js.annotation._

import eu.timepit.refined.auto._
import explore.AppMain
import explore._
import explore.components.Tile
import explore.model._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target

@JSExportTopLevel("TargetTest")
object Test extends AppMain {

  override protected def rootComponent(view: View[RootModel]): VdomElement = {
    val id = Target.Id(2L)

    <.div(^.height := "100vh",
          ^.width := "100%",
          Tile("Target", false)(
            TargetEditor(id)
          )
    )
  }

}
