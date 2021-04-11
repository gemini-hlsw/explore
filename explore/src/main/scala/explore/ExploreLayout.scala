// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all._
import explore.components.state.IfLogged
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.modules.sidebar.Sidebar
import react.semanticui.modules.sidebar.SidebarAnimation
import react.semanticui.modules.sidebar.SidebarDirection
import react.semanticui.modules.sidebar.SidebarPushable
import react.semanticui.modules.sidebar.SidebarPusher
import react.semanticui.modules.sidebar.SidebarWidth

final case class ExploreLayout(c: RouterCtl[Page], r: ResolutionWithProps[Page, View[RootModel]])(
  val view:                       View[RootModel]
) extends ReactProps[ExploreLayout](ExploreLayout.component)

object ExploreLayout {
  type Props = ExploreLayout

  private val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .render_P { p =>
        AppCtx.using { implicit ctx =>
          IfLogged(p.view) { (vault, onLogout) =>
            HelpCtx.usingView { helpCtx =>
              val helpView = helpCtx.zoom(HelpContext.displayedHelp)
              SidebarPushable(
                Sidebar(
                  width = SidebarWidth.Wide,
                  direction = SidebarDirection.Right,
                  animation = SidebarAnimation.Overlay,
                  visible = helpView.get.isDefined
                )(
                  helpView.get
                    .map { h =>
                      HelpBody(helpCtx.get, h): VdomNode
                    }
                    .when(helpView.get.isDefined)
                ),
                SidebarPusher(dimmed = helpView.get.isDefined)(
                  <.div(
                    ExploreStyles.MainGrid,
                    TopBar(vault.user, onLogout >> p.view.zoom(RootModel.vault).set(none)),
                    <.div(
                      ExploreStyles.SideTabs,
                      SideTabs(p.view.zoom(RootModel.tabs))
                    ),
                    <.div(
                      ExploreStyles.MainBody,
                      p.r.renderP(p.view)
                    )
                  )
                )
              )
            }
          }
        }
      }
      .build

}
