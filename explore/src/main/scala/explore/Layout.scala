// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.components.ui.ExploreStyles
import explore.model._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._

final case class OTLayout(c: RouterCtl[Page], r: ResolutionWithProps[Page, View[RootModel]])(
  val view:                  View[RootModel]
) extends ReactProps[OTLayout](OTLayout.component)

object OTLayout {

  private val component =
    ScalaComponent
      .builder[OTLayout]
      .stateless
      .render_P { p =>
        <.div(
          ExploreStyles.MainGrid,
          TopBar(p.view.zoom(RootModel.vault)),
          <.div(
            ExploreStyles.SideTabs,
            SideTabs(p.view.zoom(RootModel.tabs))
          ),
          <.div(
            ExploreStyles.MainBody,
            p.r.renderP(p.view)
          )
        )
      }
      .build

}
