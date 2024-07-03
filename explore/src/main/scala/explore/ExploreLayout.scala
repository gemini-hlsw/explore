// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.cache.ModesCache
import explore.cache.PreferencesCache
import explore.cache.ProgramCache
import explore.components.ui.ExploreStyles
import explore.events.ExploreEvent
import explore.events.ExploreEvent.LogoutEventId
import explore.model.*
import explore.model.AppContext
import explore.model.enums.AppTab
import explore.programs.ProgramsPopup
import explore.shortcuts.*
import explore.shortcuts.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.React
import japgolly.scalajs.react.extra.router.ResolutionWithProps
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramType
import lucuma.core.util.Display
import lucuma.react.common.*
import lucuma.react.hotkeys.*
import lucuma.react.hotkeys.hooks.*
import lucuma.react.primereact.ConfirmDialog
import lucuma.react.primereact.Message
import lucuma.react.primereact.Sidebar
import lucuma.react.primereact.Toast
import lucuma.react.primereact.ToastRef
import lucuma.react.primereact.hooks.all.*
import lucuma.refined.*
import lucuma.ui.components.SideTabs
import lucuma.ui.components.state.IfLogged
import lucuma.ui.enums.Theme
import lucuma.ui.hooks.*
import lucuma.ui.layout.LayoutStyles
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import queries.common.UserPreferencesQueriesGQL.*

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
      .useStreamOnMountBy: (_, _, ctx, toastRef) =>
        ctx.broadcastChannel.messages.evalTap(
          // This is coming from the js world, we can't match the type
          _.data.event match {
            // TODO: Handle logout events
            case ExploreEvent.PWAUpdateId =>
              // Clear other toasts first
              toastRef.clear().toAsync *>
                toastRef
                  .upgradePrompt(
                    <.span(
                      "A new version of ",
                      <.a(
                        ExploreStyles.UpgradeLink,
                        "Explore",
                        ^.href   := s"https://github.com/gemini-hlsw/explore/compare/${utils.gitHash.orEmpty}...HEAD",
                        ^.target := "_blank"
                      ),
                      " is available!"
                    ),
                    ctx.broadcastChannel
                      .postMessage(ExploreEvent.PWAReload)
                      .runAsyncAndForget
                  )
                  .toAsync
            case _                        => IO.unit
          }
        )
      .useEffectOnMountBy: (_, _, ctx, _, _) =>
        ctx.broadcastChannel.postMessage(ExploreEvent.ExploreUIReady)
      .useStateView(none[NonEmptyString]) // userSelectionMessage
      .useTheme(Theme.Dark)
      .render: (props, helpCtx, ctx, toastRef, _, userSelectionMessage, theme) =>
        import ctx.given

        // Creates a "profile" for user preferences.
        val createUserPrefs: IO[Unit] =
          props.view.get.vault.foldMap(vault =>
            UserInsertMutation[IO].execute(vault.user.id.toString.assign).start.void
          )

        val userVault = props.view.zoom(RootModel.vault)

        IfLogged[ExploreEvent](
          "Explore".refined,
          ExploreStyles.LoginTitle,
          allowGuest = true,
          ctx.sso,
          userVault,
          userSelectionMessage,
          ctx.clients.init(_),
          ctx.clients.close(),
          createUserPrefs,
          "explore".refined,
          _.event === ExploreEvent.LogoutEventId,
          _.value.toString,
          ExploreEvent.LogoutEvent(_)
        )(onLogout =>
          val routingInfo = RoutingInfo.from(props.resolution.page)

          val routingInfoView: View[RoutingInfo] =
            View(
              routingInfo,
              (mod, cb) =>
                val newRoute = mod(routingInfo)
                ctx.pushPage(newRoute.appTab, newRoute.programId, newRoute.focused) >> cb(newRoute)
            )

          val helpView = helpCtx.displayedHelp

          given Display[AppTab] = _.title

          val (showProgsPopup, msg, isSubmitted, ref) =
            props.view.get.programSummaries.fold((false, none, false, none)) { pss =>
              routingInfo.optProgramId.fold((true, none, false, none)) { id =>
                if (pss.programs.get(id).exists(!_.deleted))
                  (false, none, pss.proposalIsSubmitted, pss.proposalId)
                else
                  (true,
                   s"The program id in the url, '$id', either does not exist, is deleted, or you do not have authorization to view it.".some,
                   false,
                   none
                  )
              }
            }

          React.Fragment(
            ConfirmDialog(),
            Toast(Toast.Position.BottomRight, baseZIndex = 2000).withRef(toastRef.ref),
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
            <.div(LayoutStyles.MainGrid)(
              // This might use the `RoutingInfo.dummyProgramId` if the URL had no
              // no program id in it. But, that's OK, because the list of user
              // programs will still load and they will be redirected to the program
              // selection popup.
              ModesCache(props.view.zoom(RootModel.spectroscopyModes).async.set),
              ProgramCache(
                routingInfo.programId,
                props.view.zoom(RootModel.user).get.map(_.role.name),
                props.view.zoom(RootModel.programSummaries).async.set
              ),
              userVault.mapValue: (vault: View[UserVault]) =>
                React.Fragment(
                  PreferencesCache(
                    vault.get.user.id,
                    props.view.zoom(RootModel.userPreferences).async.set
                  ),
                  TopBar(
                    vault,
                    routingInfo.optProgramId,
                    props.view.zoom(RootModel.localPreferences).get,
                    props.view.zoom(RootModel.undoStacks),
                    props.view
                      .zoom(RootModel.programSummaries.some)
                      .zoom(ProgramSummaries.programs),
                    theme,
                    onLogout >> props.view.zoom(RootModel.vault).set(none).toAsync
                  )
                ),
              SideTabs(
                "side-tabs".refined,
                routingInfoView.zoom(RoutingInfo.appTab),
                ctx.pageUrl(_, routingInfo.programId, routingInfo.focused),
                _.separatorAfter,
                tab =>
                  props.view.get.programSummaries
                    .flatMap(_.optProgramDetails)
                    .map(_.programType === ProgramType.Science || tab =!= AppTab.Proposal)
                    .getOrElse(true)
              ),
              if (showProgsPopup)
                ProgramsPopup(
                  currentProgramId = none,
                  props.view
                    .zoom(RootModel.programSummaries.some)
                    .zoom(ProgramSummaries.programs),
                  undoStacks = props.view.zoom(RootModel.undoStacks),
                  onLogout = (onLogout >>
                    props.view.zoom(RootModel.vault).set(none).toAsync).some,
                  message = msg
                ): VdomElement
              else
                <.div(LayoutStyles.MainBody, LayoutStyles.WithMessage.when(isSubmitted))(
                  props.resolution.renderP(props.view),
                  if (isSubmitted)
                    Message(text =
                      s"The proposal has been submitted as ${ref.foldMap(_.label)} and may be retracted to allow modifications until the proposal deadline."
                    )
                  else EmptyVdom
                )
            )
          )
        )
