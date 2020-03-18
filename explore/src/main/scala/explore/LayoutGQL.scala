// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.model._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.router._
import react.semanticui.collections.menu._
import react.common._

final case class OTLayoutGQL(c: RouterCtl[Page], r: Resolution[Page])(val model: RootModel)
    extends ReactProps {

  @inline def render: VdomElement = OTLayoutGQL.component(this)
}

object OTLayoutGQL {

  private val component =
    ScalaComponent
      .builder[OTLayoutGQL]("DemoGQL")
      .render_P { p =>
        <.div(
          ^.cls := "theme dimmable",
          Menu(
            attached   = MenuAttached.Top,
            compact    = true,
            borderless = true,
            tabular    = MenuTabular.Right
          )(
            MenuItem(as = "div")(
              Icons.BarsIcon,
              "Explore"
            )
          ),
          p.r.render()
        )
      }
      .build

}
