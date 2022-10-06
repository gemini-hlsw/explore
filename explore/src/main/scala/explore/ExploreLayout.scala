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
import explore.model.enums.AppTab
import explore.shortcuts.*
import explore.shortcuts.given
import explore.syntax.ui.*
import explore.syntax.ui.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.ResolutionWithProps
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.hotkeys.*
import react.hotkeys.hooks.*
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
      .useContext(AppContext.ctx)
      .useGlobalHotkeysWithDepsBy((props, _, _) => props.resolution.page.toString) {
        (props, _, ctx) => _ =>
          val routingInfo          = RoutingInfo.from(props.resolution.page)
          def goToTab(tab: AppTab) =
            ctx.setPageVia(tab, routingInfo.programId, routingInfo.focused, SetRouteVia.HistoryPush)

          val callbacks: ShortcutCallbacks = {
            case GoToObs         =>
              goToTab(AppTab.Observations)
            case GoToTargets     =>
              goToTab(AppTab.Targets)
            case GoToProposals   =>
              goToTab(AppTab.Proposal)
            case GoToConstraints =>
              goToTab(AppTab.Constraints)
            case GoToOverview    =>
              goToTab(AppTab.Overview)
          }
          UseHotkeysProps(
            List(GoToObs, GoToTargets, GoToProposals, GoToConstraints, GoToOverview).toHotKeys,
            callbacks
          )
      }
      .render { (props, helpCtx, _) =>
        IfLogged(props.view)((vault: UserVault, onLogout: IO[Unit]) =>
          val routingInfo = RoutingInfo.from(props.resolution.page)

          val helpView = helpCtx.displayedHelp
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
      }
