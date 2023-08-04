// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.fa.IconSize
import lucuma.ui.syntax.all.given

object UnderConstruction:
  protected val component =
    ScalaComponent
      .builder[Unit]
      .stateless
      .render { _ =>
        <.div(
          ExploreStyles.HVCenter,
          <.div(
            <.div("Under Construction"),
            <.div(
              ExploreStyles.HVCenter,
              Icons.Gears
                .withSize(IconSize.X5)
                .withTitle("Under construction")
            )
          )
        )
      }
      .build

  def apply() = component()
