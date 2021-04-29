// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import explore.components.state.IfLogged
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model._
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
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

  implicit val resolutionWithPropsReuse: Reusability[ResolutionWithProps[Page, View[RootModel]]] =
    Reusability.byRefOr_==
  implicit val propsReuse: Reusability[Props]                                                    =
    Reusability.by(p => (p.r, p.view))

  final case class State(helpVisible: Boolean)
  implicit val stateReuse: Reusability[State] =
    Reusability.derive

  private def renderFn($ : RenderScope[Props, State, Unit])(
    props:                Props,
    vault:                UserVault,
    onLogout:             IO[Unit]
  ): VdomNode =
    AppCtx.using { implicit ctx =>
      HelpCtx.usingView { helpCtx =>
        val helpView = helpCtx.zoom(HelpContext.displayedHelp)
        SidebarPushable(
          Sidebar(
            onShow = $.setState(State(true)),
            onHidden = $.setState(State(false)),
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
              TopBar(vault.user, onLogout >> props.view.zoom(RootModel.vault).set(none)),
              <.div(
                ExploreStyles.SideTabs,
                SideTabs(props.view.zoom(RootModel.tabs))
              ),
              <.div(
                ExploreStyles.MainBody,
                props.r.renderP(props.view)
              )
            )
          )(
            ^.onClick -->
              helpView.set(none).runAsyncAndForgetCB.when_($.state.helpVisible)
          )
        )
      }
    }

  private val component =
    ScalaComponent
      .builder[Props]
      .initialState(State(false))
      .renderP { ($, p) =>
        IfLogged(p.view)((renderFn($) _).reusable(p))
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

}
