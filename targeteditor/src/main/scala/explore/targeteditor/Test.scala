// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import eu.timepit.refined.auto._
import explore.AppMain
import explore._
import explore.components.Tile
import explore.components.state.IfLogged
import explore.model._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.core.model.User

import scala.scalajs.js.annotation._

@JSExportTopLevel("TargetTest")
object Test extends AppMain {

  override protected def rootComponent(view: View[RootModel]): VdomElement = {
    val uid = User.Id(112L)
    val id  = Target.Id(2L)

    IfLogged(view)((_, _) =>
      <.div(^.height := "100vh",
            ^.width := "100%",
            Tile("Target")(
              TargetEditor(uid, id)
            )
      )
    )
  }

}
