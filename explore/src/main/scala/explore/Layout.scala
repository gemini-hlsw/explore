// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.model._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.As
import react.semanticui.collections.menu._
import react.semanticui.modules.sidebar._

final case class OTLayout(c: RouterCtl[Page], r: ResolutionWithProps[Page, ViewCtxIO[RootModel]])(
  val viewCtxIO:             ViewCtxIO[RootModel]
) extends ReactProps {

  @inline def render: VdomElement = OTLayout.component(this)
}

object OTLayout {

  final case class State(menu: Boolean)

  private val component =
    ScalaComponent
      .builder[OTLayout]
      .initialState(State(true))
      .renderPS { ($, p, s) =>
        <.div(
          ^.cls := "theme dimmable",
          Menu(
            attached = MenuAttached.Top,
            compact = true,
            borderless = true,
            tabular = MenuTabular.Right
          )(
            MenuItem(as = "a", onClick = $.modState((s: State) => s.copy(menu = !s.menu)))(
              Icons.BarsIcon,
              "Explore"
            )
          ),
          SidebarPushable(className = "maingrid")(
            Sidebar(
              as = As.Menu(
                Menu(
                  inverted = true,
                  vertical = true,
                  icon = MenuIcon.Labeled
                )
              ),
              width = SidebarWidth.Wide,
              animation = SidebarAnimation.Push,
              direction = SidebarDirection.Left,
              visible = s.menu
            )(
              MenuHeader()(
                <.div(
                  p.viewCtxIO.value.get.obsId.map(_.format).getOrElse[String]("No Observation")
                ),
                <.div(
                  p.viewCtxIO.value.get.target
                    .map(_.name)
                    .getOrElse[String]("No Target")
                )
              ),
              MenuItem(as = "a", className = "sidetab")("Targets"),
              MenuItem(as = "a", className = "sidetab")("P II")
            ),
            SidebarPusher(dimmed = false)(
              p.r.renderP(p.viewCtxIO)
            )
          )
        )
      }
      .build

}
