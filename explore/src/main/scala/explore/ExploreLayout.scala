// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.implicits.*
import explore.components.state.IfLogged
import explore.components.ui.ExploreStyles
import explore.model.*
import explore.syntax.ui.*
import explore.syntax.ui.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.ResolutionWithProps
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.*
import react.hotkeys.*
import react.semanticui.modules.sidebar.Sidebar
import react.semanticui.modules.sidebar.SidebarAnimation
import react.semanticui.modules.sidebar.SidebarDirection
import react.semanticui.modules.sidebar.SidebarPushable
import react.semanticui.modules.sidebar.SidebarPusher
import react.semanticui.modules.sidebar.SidebarWidth

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

case class ExploreLayout(
  resolution: ResolutionWithProps[Page, View[RootModel]]
)(
  val view:   View[RootModel]
) extends ReactFnProps(ExploreLayout.component)

object ExploreLayout:
  private type Props = ExploreLayout

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(HelpContext.ctx)
      .render { (props, helpCtx) =>
        IfLogged(props.view)((vault: UserVault, onLogout: IO[Unit]) =>
          val routingInfo = RoutingInfo.from(props.resolution.page)

          val helpView = helpCtx.displayedHelp
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
                    HelpBody(helpCtx, h)
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
                    props.resolution.renderP(props.view)
                  )
                )
              )(
                ^.onClick --> helpView.set(none)
              )
            )
          )
        )
      }
