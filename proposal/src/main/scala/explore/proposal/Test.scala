// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import scala.scalajs.js

import eu.timepit.refined.auto._
import explore.AppMain
import explore.View
import explore.components.state.IfLogged
import explore.model.RootModel
import japgolly.scalajs.react.vdom.VdomElement

import js.annotation._

@JSExportTopLevel("PropsTest")
object Test extends AppMain {

  override protected def rootComponent(rootView: View[RootModel]): VdomElement =
    IfLogged(rootView)((_, _) => ProposalTabContents(rootView.zoom(RootModel.focused)))
}
