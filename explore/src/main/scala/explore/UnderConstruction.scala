// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.components.ui.GPPStyles
import explore.Icons.CogsIcon
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.sizes.Huge

object UnderConstruction {

  protected val component =
    ScalaComponent
      .builder[Unit]
      .stateless
      .render { _ =>
        <.div(
          GPPStyles.HVCenter,
          <.div(
            <.div("Under Construction"),
            <.div(GPPStyles.HVCenter, CogsIcon.copy(size = Huge))
          )
        )
      }
      .build

  def apply() = component()

}
