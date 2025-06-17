// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import crystal.syntax.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.cache.CfpCacheController
import explore.cache.ModesCacheController
import explore.cache.PreferencesCacheController
import explore.cache.ProgramCacheController
import explore.cache.ResetType
import explore.components.ui.ExploreStyles
import explore.events.ExploreEvent
import explore.model.*
import explore.model.enums.AppTab
import explore.programs.ProgramsPopup
import explore.shortcuts.*
import explore.shortcuts.given
import explore.syntax.ui.*
import explore.utils.*
import fs2.dom.BroadcastChannel
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.ResolutionWithProps
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramType
import lucuma.core.model.ProposalReference
import lucuma.core.util.Display
import lucuma.core.util.Timestamp
import lucuma.react.common.*
import lucuma.react.hotkeys.*
import lucuma.react.hotkeys.hooks.*
import lucuma.react.primereact.ConfirmDialog
import lucuma.react.primereact.Sidebar
import lucuma.react.primereact.Toast
import lucuma.react.primereact.ToastRef
import lucuma.react.primereact.hooks.all.*
import lucuma.refined.*
import lucuma.schemas.enums.ProposalStatus
import lucuma.ui.components.SideTabs
import lucuma.ui.components.SolarProgress
import lucuma.ui.components.state.IfLogged
import lucuma.ui.enums.Theme
import lucuma.ui.hooks.*
import lucuma.ui.layout.LayoutStyles
import lucuma.ui.reusability.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom.document
import queries.common.UserPreferencesQueriesGQL.*

case class ExploreLayout(
  resolution: ResolutionWithProps[Page, RootModelViews]
)(
  val model:  RootModelViews
) extends ReactFnProps(ExploreLayout.component)

object ExploreLayout:
  private type Props = ExploreLayout

  private given Reusability[ToastRef] = Reusability.by_==

  private def useMountHotkeys(ctx: AppContext[IO], helpCtx: HelpContext)(
    routingInfo: Option[RoutingInfo]
  ): HookResult[Unit] =
    useGlobalHotkeysWithDeps(routingInfo): ri =>
      ri.map: routingInfo =>
        def goToTab(tab: AppTab) =
          ctx.setPageVia(
            (tab, routingInfo.programId, routingInfo.focused).some,
            SetRouteVia.HistoryPush
          )

        val callbacks: ShortcutCallbacks =
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
            helpCtx.displayedHelp.set(Some("shortcuts.md".refined))

        UseHotkeysProps(
          (ShortcutsHelpKeys :::
            List(
              GoToObs,
              GoToTargets,
              GoToProposals,
              GoToConstraints,
              GoToOverview
            )).toHotKeys,
          callbacks
        )
      .getOrElse(UseHotkeysProps(List.empty, Callback.empty))

  private def useMountProgressiveWebApp(
    bc:       BroadcastChannel[IO, ExploreEvent],
    toastRef: ToastRef
  ): HookResult[Unit] =
    useEffectStreamOnMount:
      bc.messages.evalMap:
        // This is coming from the js world, we can't match the type
        _.data.event match
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
                  bc
                    .postMessage(ExploreEvent.PWAReload)
                    .runAsyncAndForget
                )
                .toAsync
          case _                        => IO.unit

  private val component =
    ScalaFnComponent[Props]: props =>
      for
        helpCtx              <- useContext(HelpContext.ctx)
        ctx                  <- useContext(AppContext.ctx)
        _                    <- useEffectWithDeps(
                                  props.model.programSummariesValue.toOption.flatMap(_.programOrProposalReference)
                                ):
                                  // Set the title of the page to the program reference
                                  // scalajs-react suggest to use setTitle for this but we'd need to put the
                                  // reference in the url for that
                                  _.fold(Callback(document.title = "Explore")): r =>
                                    Callback(document.title = s"Explore - ${r}")
        routingInfo           = RoutingInfo.from(props.resolution.page)
        _                    <- useMountHotkeys(ctx, helpCtx)(routingInfo)
        toastRef             <- useToastRef
        _                    <- useEffectWithDeps(toastRef):
                                  import ctx.given

                                  toastRef =>
                                    toastRef.ref.get
                                      .flatMap:
                                        _.map(_ => ctx.toastRef.complete(toastRef).void.runAsync).orEmpty
        _                    <- useMountProgressiveWebApp(ctx.broadcastChannel, toastRef)
        _                    <- useEffectOnMount:
                                  ctx.broadcastChannel.postMessage(ExploreEvent.ExploreUIReady)
        userSelectionMessage <- useStateView(none[NonEmptyString])
        theme                <- useTheme(Theme.Dark)
        // Indicates whether the current program has errored.
        // We keep a separate state from the Pot in props.model.programSummaries so that we can keep
        // the value there when there's an error, and show the error only as a modal on top.
        programError         <- useState(none[String])
        // Reset the program cache when the program changes.
        _                    <- useEffectWithDeps(routingInfo.map(_.programId)): _ =>
                                  ctx.resetProgramCache(none)
        // Reset the program cache when there's an error signal.
        _                    <- useEffectStreamResourceOnMount:
                                  ctx.resetProgramCacheTopic.subscribeAwaitUnbounded.map:
                                    _.unNone.evalMap: errorMsg =>
                                      programError.setStateAsync(errorMsg.some)
      yield
        import ctx.given

        val view: View[RootModel] = props.model.rootModel

        // Creates a "profile" for user preferences.
        val createUserPrefs: IO[Unit] =
          view.get.vault.foldMap(vault =>
            UserInsertMutation[IO].execute(vault.user.id.toString.assign).start.void
          )

        val userVault: View[Option[UserVault]] = view.zoom(RootModel.vault)

        React.Fragment(
          Toast(Toast.Position.BottomRight, baseZIndex = 2000).withRef(toastRef.ref),
          // On Error we show a modal dialog with the error message while we reload.
          programError.value
            .map(error =>
              React.Fragment(
                Sidebar(
                  onHide = Callback.empty,
                  visible = true,
                  modal = true,
                  showCloseIcon = false,
                  dismissable = false,
                  position = Sidebar.Position.Bottom,
                  content = error,
                  clazz = ExploreStyles.GlobalErrorDialog
                ),
                SolarProgress()
              )
            ),
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
            routingInfo
              .map { routingInfo =>
                val routingInfoView: View[RoutingInfo] =
                  View(
                    routingInfo,
                    (mod, cb) =>
                      val oldRoute = routingInfo
                      val newRoute = mod(routingInfo)
                      ctx.pushPage((newRoute.appTab, newRoute.programId, newRoute.focused).some) >>
                        cb(oldRoute, newRoute)
                  )

                val helpView: View[Option[NonEmptyString]] = helpCtx.displayedHelp

                given Display[AppTab] = _.title

                val programSummaries: Pot[ProgramSummaries] =
                  props.model.programSummariesValue

                val (showProgsPopupPot, msg, isSubmitted, proposalReference)
                  : (Pot[Boolean], Option[String], Boolean, Option[ProposalReference]) =
                  programSummaries.toOption.fold((pending, none, false, none)): pss =>
                    routingInfo.optProgramId.fold((true.ready, none, false, none)): id =>
                      if (pss.programs.get(id).exists(!_.deleted))
                        (false.ready, none, pss.proposalIsSubmitted, pss.proposalId)
                      else
                        (true.ready,
                         s"The program id in the url, '$id', either does not exist, is deleted, or you do not have authorization to view it.".some,
                         false,
                         none
                        )

                val deadline: Option[Timestamp] =
                  programSummaries.toOption
                    .flatMap: programSummaries =>
                      ProgramSummaries.proposal
                        .getOption(programSummaries)
                        .flatten
                        .flatMap: p =>
                          val piP = ProgramSummaries.piPartner.getOption(programSummaries)
                          p.deadline(piP)

                val cacheKey: String =
                  userVault.get
                    .map(_.user)
                    .foldMap(u => s"${routingInfo.programId}-${u.id}-${u.role.name}")

                React.Fragment(
                  ConfirmDialog(),
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
                    ModesCacheController(view.zoom(RootModel.scienceModes).async.mod),
                    CfpCacheController(view.zoom(RootModel.cfps).async.mod),
                    // This might use the `RoutingInfo.dummyProgramId` if the URL had no
                    // program id in it. But, that's OK, because the list of user
                    // programs will still load and they will be redirected to the program
                    // selection popup.
                    ProgramCacheController(
                      routingInfo.programId,
                      props.model.programSummaries.throttledView.mod,
                      programError.setState(none).toAsync,
                      ctx.resetProgramCacheTopic.subscribeUnbounded // On error, keep the current program cache.
                        .map(_.fold(ResetType.Wipe)(_ => ResetType.Keep))
                    ),
                    userVault.mapValue: (vault: View[UserVault]) =>
                      React.Fragment(
                        PreferencesCacheController(
                          vault.get.user.id,
                          view.zoom(RootModel.userPreferences).async.mod
                        ).withKey(cacheKey),
                        props.model.rootModel
                          .zoom:
                            RootModel.userPreferences
                              .andThen(Pot.readyPrism)
                              .andThen(UserPreferences.globalPreferences)
                          .asView
                          .map(prefs =>
                            TopBar(
                              vault,
                              routingInfo.optProgramId,
                              programSummaries.toOption
                                .flatMap(_.programOrProposalReference),
                              view.zoom(RootModel.localPreferences).get,
                              view.zoom(RootModel.undoStacks),
                              props.model.programSummaries.throttlerView
                                .zoom(Pot.readyPrism)
                                .zoom(ProgramSummaries.programs),
                              theme,
                              onLogout >> view.zoom(RootModel.vault).set(none).toAsync,
                              _ => ctx.resetProgramCache(none),
                              prefs
                            )
                          ),
                        showProgsPopupPot.renderPot: showProgsPopup =>
                          if (showProgsPopup)
                            ProgramsPopup(
                              currentProgramId = none,
                              vault.get.user.id,
                              vault.get.isStaff,
                              props.model.programSummaries.throttlerView
                                .zoom(Pot.readyPrism)
                                .zoom(ProgramSummaries.programs),
                              undoStacks = view.zoom(RootModel.undoStacks),
                              message = msg
                            ): VdomElement
                          else
                            React.Fragment(
                              SideTabs(
                                "side-tabs".refined,
                                routingInfoView.zoom(RoutingInfo.appTab),
                                tab =>
                                  ctx.pageUrl(
                                    (tab, routingInfo.programId, routingInfo.focused).some
                                  ),
                                _.separatorAfter,
                                tab =>
                                  programSummaries.toOption
                                    .flatMap(_.optProgramDetails)
                                    .forall: program =>
                                      // Only show Program and Proposal tabs for Science proposals, and Program only for Accepted ones
                                      (tab =!= AppTab.Proposal && tab =!= AppTab.Program) ||
                                        program.programType === ProgramType.Science &&
                                        (tab === AppTab.Proposal || program.proposalStatus === ProposalStatus.Accepted)
                              ),
                              <.div(
                                LayoutStyles.MainBody,
                                LayoutStyles.WithMessage.when(isSubmitted)
                              )(
                                props.resolution.renderP(props.model),
                                TagMod.when(isSubmitted):
                                  SubmittedProposalMessage(proposalReference, deadline)
                              )
                            )
                      )
                  )
                )
              }
              .getOrElse:
                props.resolution.renderP(props.model)
          )
        )
