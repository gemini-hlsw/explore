// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.implicits.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.state.IfLogged
import explore.components.ui.ExploreStyles
import explore.events.ExploreEvent
import explore.model.*
import explore.model.enums.AppTab
import explore.shortcuts.*
import explore.shortcuts.given
import explore.syntax.ui.*
import explore.syntax.ui.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.ResolutionWithProps
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.broadcastchannel.*
import lucuma.refined.*
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.*
import react.fa.IconSize
import react.hotkeys.*
import react.hotkeys.hooks.*
import react.primereact.MessageItem
import react.primereact.Sidebar
import react.primereact.Toast
import react.primereact.ToastRef
import react.primereact.hooks.all.*

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import explore.cache.ProgramCache

case class ExploreLayout(
  resolution: ResolutionWithProps[Page, View[RootModel]]
)(
  val view:   View[RootModel]
) extends ReactFnProps(ExploreLayout.component)

object ExploreLayout:
  private type Props = ExploreLayout

  private given Reusability[ToastRef] = Reusability.by_==

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(HelpContext.ctx)
      .useContext(AppContext.ctx)
      .useGlobalHotkeysWithDepsBy((props, _, _) => props.resolution.page.toString) {
        (props, help, ctx) => _ =>
          val routingInfo          = RoutingInfo.from(props.resolution.page)
          def goToTab(tab: AppTab) =
            ctx.setPageVia(tab, routingInfo.programId, routingInfo.focused, SetRouteVia.HistoryPush)

          val callbacks: ShortcutCallbacks = {
            case GoToObs                         =>
              goToTab(AppTab.Observations)
            case GoToTargets                     =>
              goToTab(AppTab.Targets)
            case GoToProposals                   =>
              goToTab(AppTab.Proposal)
            case GoToConstraints                 =>
              goToTab(AppTab.Constraints)
            case GoToOverview                    =>
              goToTab(AppTab.Overview)
            case ShortcutsHelp1 | ShortcutsHelp2 =>
              help.displayedHelp.set(Some("shortcuts.md".refined))
          }
          UseHotkeysProps(
            (ShortcutsHelpKeys :::
              List(GoToObs, GoToTargets, GoToProposals, GoToConstraints, GoToOverview)).toHotKeys,
            callbacks
          )
      }
      .useToastRef
      .useEffectWithDepsBy((_, _, _, toastRef) => toastRef)((_, _, ctx, _) =>
        import ctx.given

        toastRef =>
          toastRef.ref.get
            .flatMap(
              _.map(_ => ctx.toastRef.complete(toastRef).void.runAsync).orEmpty
            )
      )
      .useEffectOnMountBy { (props, _, ctx, toastRef) =>
        Callback {
          ctx.broadcastChannel.onmessage = (
            (x: ExploreEvent) =>
              // This is coming from the js world, we can't match the type
              x.event match {
                // TODO: Handle logout events
                case ExploreEvent.PWAUpdateId =>
                  toastRef
                    .prompt(
                      "A new version of explore is available!",
                      IO(
                        ctx.broadcastChannel.postMessage(ExploreEvent.PWAReload)
                      ).runAsyncAndForget
                    )
                    .to[IO]
                case _                        => IO.unit
              }
          ): (ExploreEvent => IO[Unit])
          // Scala 3 infers the return type as Any if we don't ascribe

          ctx.broadcastChannel.postMessage(ExploreEvent.ExploreUIReady)
        }
      }
      .render { (props, helpCtx, ctx, toastRef) =>
        import ctx.given

        IfLogged(props.view)((vault: UserVault, onLogout: IO[Unit]) =>
          val routingInfo = RoutingInfo.from(props.resolution.page)

          val helpView = helpCtx.displayedHelp

          React.Fragment(
            Toast(Toast.Position.BottomRight).withRef(toastRef.ref),
            Sidebar(
              position = Sidebar.Position.Right,
              size = Sidebar.Size.Medium,
              visible = helpView.get.isDefined,
              clazz = ExploreStyles.HelpSidebar,
              showCloseIcon = false,
              onHide = helpView.set(none).when_(helpView.get.isDefined)
            )(
              <.div(
                helpView.get
                  .map(h => HelpBody(helpCtx, h))
                  .when(helpView.get.isDefined)
              )
            ),
            <.div(
              ExploreStyles.MainGrid,
              TopBar(
                vault,
                routingInfo.optProgramId,
                props.view.zoom(RootModel.localPreferences).get,
                props.view.zoom(RootModel.undoStacks),
                onLogout >> props.view.zoom(RootModel.vault).set(none).to[IO]
              ),
              routingInfo.optProgramId
                .map(programId =>
                  ProgramCache.Provider(ProgramCache(programId))(
                    <.div(
                      ExploreStyles.SideTabs,
                      SideTabs(routingInfo)
                    ),
                    <.div(
                      ExploreStyles.MainBody,
                      props.resolution.renderP(props.view)
                    )
                  )
                )
                .whenDefined
            )
          )
        )
      }
