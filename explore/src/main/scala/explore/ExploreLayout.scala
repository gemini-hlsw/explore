// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import crystal.react.implicits._
import explore.components.state.IfLogged
import explore.components.ui.ExploreStyles
import explore.model._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.hotkeys._
import react.semanticui.modules.sidebar.Sidebar
import react.semanticui.modules.sidebar.SidebarAnimation
import react.semanticui.modules.sidebar.SidebarDirection
import react.semanticui.modules.sidebar.SidebarPushable
import react.semanticui.modules.sidebar.SidebarPusher
import react.semanticui.modules.sidebar.SidebarWidth

import scala.scalajs.js

final case class ExploreLayout(
  c:        RouterCtl[Page],
  r:        ResolutionWithProps[Page, View[RootModel]]
)(
  val view: View[RootModel]
) extends ReactFnProps[ExploreLayout](ExploreLayout.component)

object ExploreLayout {
  type Props = ExploreLayout

  private val component =
    ScalaFnComponent[Props] { props =>
      IfLogged(props.view)(
        (
          vault:    UserVault,
          onLogout: IO[Unit]
        ) =>
          AppCtx.using { implicit ctx =>
            val routingInfo = RoutingInfo.from(props.r.page)

            HelpCtx.usingView { helpCtx =>
              val helpView = helpCtx.zoom(HelpContext.displayedHelp)
              GlobalHotKeys(
                keyMap = KeyMap("CLOSE_HELP" -> "ESC"),
                handlers = Handlers("CLOSE_HELP" -> helpView.set(none))
              )(
                SidebarPushable(
                  Sidebar(
                    width = SidebarWidth.Wide,
                    direction = SidebarDirection.Right,
                    animation = SidebarAnimation.Overlay,
                    visible = helpView.get.isDefined
                  )(
                    helpView.get
                      .map { h =>
                        // Lazy load the React component for help
                        val prom = js.dynamicImport {
                          new HelpLoader().loadHelp(helpCtx.get, h)
                        }
                        React.Suspense(<.div("Loading"), AsyncCallback.fromJsPromise(prom))
                      }
                      .when(helpView.get.isDefined)
                  ),
                  SidebarPusher(dimmed = helpView.get.isDefined)(
                    <.div(
                      ExploreStyles.MainGrid,
                      TopBar(
                        vault.user,
                        routingInfo.optProgramId,
                        props.view.zoom(RootModel.localPreferences).get,
                        props.view.zoom(RootModel.undoStacks),
                        onLogout >> props.view.zoom(RootModel.vault).set(none).to[IO]
                      ),
                      <.div(
                        ExploreStyles.SideTabs,
                        SideTabs(routingInfo)
                      ),
                      <.div(
                        ExploreStyles.MainBody,
                        props.r.renderP(props.view)
                      )
                    )
                  )(
                    ^.onClick --> helpView.set(none)
                  )
                )
              )
            }
          }: VdomNode
      )
    }

}
