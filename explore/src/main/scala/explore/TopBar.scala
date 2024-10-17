// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.View
import crystal.react.hooks.*
import explore.components.ConnectionsStatus
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ExploreLocalPreferences
import explore.model.ExploreLocalPreferences.*
import explore.model.GlobalPreferences
import explore.model.ProgramInfoList
import explore.model.ProgramSummaries
import explore.programs.ProgramsPopup
import explore.undo.UndoStacks
import explore.users.RedeemInvitationsPopup
import explore.users.UserPreferencesPopup
import japgolly.scalajs.react.*
import japgolly.scalajs.react.callback.CallbackCatsEffect.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.model.GuestRole
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.react.common.*
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.Button
import lucuma.react.primereact.Image
import lucuma.react.primereact.MenuItem
import lucuma.react.primereact.PopupTieredMenu
import lucuma.react.primereact.Toolbar
import lucuma.react.primereact.hooks.all.*
import lucuma.refined.*
import lucuma.ui.Resources
import lucuma.ui.components.About
import lucuma.ui.components.LoginStyles
import lucuma.ui.components.ThemeSubMenu
import lucuma.ui.enums.Theme
import lucuma.ui.layout.LayoutStyles
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import org.scalajs.dom
import org.scalajs.dom.window
import typings.loglevel.mod.LogLevelDesc

case class TopBar(
  vault:                      View[UserVault],
  programId:                  Option[Program.Id],
  programOrProposalReference: Option[String],
  preferences:                ExploreLocalPreferences,
  undoStacks:                 View[UndoStacks[IO, ProgramSummaries]],
  programInfos:               ViewOpt[ProgramInfoList],
  theme:                      View[Theme],
  onLogout:                   IO[Unit],
  globalPreferences:          View[GlobalPreferences]
) extends ReactFnProps(TopBar.component)

object TopBar:
  private type Props = TopBar

  private object IsAboutOpen extends NewType[Boolean]

  private object IsProgramOpen extends NewType[Boolean]

  private object IsUserPropertiesOpen extends NewType[Boolean]

  private object IsReedemInvitationsOpen extends NewType[Boolean]

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useState(IsProgramOpen(false))
      .useStateView(IsAboutOpen(false))
      .useState(IsUserPropertiesOpen(false))
      .useState(IsReedemInvitationsOpen(false))
      .usePopupMenuRef
      .render:
        (
          props,
          ctx,
          isProgramsOpen,
          isAboutOpen,
          isUserPropertiesOpen,
          isReedemInvitationsOpen,
          menuRef
        ) =>
          import ctx.given

          val user  = props.vault.get.user
          val role  = user.role
          val level = props.preferences.level

          def logout: IO[Unit] = ctx.sso.logout >> props.onLogout

          def setLogLevel(l: LogLevelDesc): Callback =
            (ExploreLocalPreferences
              .storePreferences[IO](
                props.preferences.copy(level = l)
              ) *> IO(window.location.reload())).runAsync

          val firstItems = List(
            MenuItem.Item(
              label = "About Explore",
              icon = Icons.Info,
              command = isAboutOpen.set(IsAboutOpen(true))
            ),
            MenuItem.Item(
              label = "Manage Programs",
              icon = Icons.ListCheck,
              command = isProgramsOpen.setState(IsProgramOpen(true))
            )
          )

          val lastItems = List(
            MenuItem.Separator.some,
            MenuItem
              .Item(
                label = "Login with ORCID",
                icon = Image(
                  src = Resources.OrcidLogo,
                  clazz = ExploreStyles.OrcidIconMenu |+| LoginStyles.LoginOrcidIcon
                ),
                visible = role === GuestRole,
                command = ctx.sso.switchToORCID.runAsync
              )
              .some,
            MenuItem.Item(label = "Logout", icon = Icons.Logout, command = logout.runAsync).some,
            MenuItem
              .SubMenu(
                label = "Log Level",
                icon = Icons.BarCodeRead,
                visible = ctx.environment =!= ExecutionEnvironment.Production
              )(
                MenuItem.Item(
                  label = "Info",
                  command = setLogLevel(LogLevelDesc.INFO),
                  disabled = level =!= LogLevelDesc.DEBUG,
                  icon = Icons.Info
                ),
                MenuItem.Item(
                  label = "Debug",
                  command = setLogLevel(LogLevelDesc.DEBUG),
                  disabled = level === LogLevelDesc.DEBUG,
                  icon = Icons.Bug
                ),
                MenuItem.Item(
                  label = "Trace",
                  command = setLogLevel(LogLevelDesc.TRACE),
                  disabled = level === LogLevelDesc.TRACE,
                  icon = Icons.Pencil
                )
              )
              .some,
            if (ctx.environment === ExecutionEnvironment.Development)
              ThemeSubMenu(props.theme).some
            else
              None,
            MenuItem
              .Item(
                label = "Toggle Reusability",
                icon = Icons.CrystalBall,
                command = utils.toggleReusabilityOverlay[CallbackTo](),
                visible = ctx.environment === ExecutionEnvironment.Development
              )
              .some
          ).flattenOption

          val menuItems =
            if (role =!= GuestRole) {
              firstItems :::
                List(
                  MenuItem
                    .Item(
                      label = "User Preferences",
                      icon = Icons.UserGears,
                      command = isUserPropertiesOpen.setState(IsUserPropertiesOpen(true))
                    ),
                  MenuItem
                    .Item(
                      label = "Redeem invitations",
                      icon = Icons.UserGroupSimple,
                      command = isReedemInvitationsOpen.setState(IsReedemInvitationsOpen(true))
                    )
                ) ::: lastItems
            } else firstItems ::: lastItems

          React.Fragment(
            Toolbar(
              clazz = LayoutStyles.MainHeader,
              left = React.Fragment(
                <.span(LayoutStyles.MainTitle, s"Explore"),
                props.programOrProposalReference.map { r =>
                  React.Fragment(<.span(LayoutStyles.MainTitle, "- "),
                                 <.span(ExploreStyles.MainTitleProgramId, r)
                  )
                }
              ),
              right = React.Fragment(
                <.span(LayoutStyles.MainUserName)(user.displayName),
                RoleSwitch(props.vault, ctx.sso),
                ConnectionsStatus(),
                Button(
                  icon = Icons.Bars,
                  text = true,
                  severity = Button.Severity.Secondary,
                  onClickE = menuRef.toggle
                )
              )
            ),
            PopupTieredMenu(model = menuItems).withRef(menuRef.ref),
            if (isAboutOpen.get.value)
              About(
                "Explore".refined,
                ExploreStyles.LoginTitle,
                ctx.version,
                isAboutOpen.as(IsAboutOpen.value)
              )
            else
              EmptyVdom,
            if (isProgramsOpen.value.value)
              ProgramsPopup(
                props.programId,
                props.programInfos,
                props.undoStacks,
                isProgramsOpen.setState(IsProgramOpen(false)).some
              )
            else EmptyVdom,
            if (isUserPropertiesOpen.value.value)
              UserPreferencesPopup(
                props.vault.get,
                isUserPropertiesOpen.setState(IsUserPropertiesOpen(false)).some,
                props.globalPreferences.zoom(GlobalPreferences.wavelengthUnits)
              )
            else EmptyVdom,
            if (isReedemInvitationsOpen.value.value)
              RedeemInvitationsPopup(
                props.vault.get,
                isReedemInvitationsOpen.setState(IsReedemInvitationsOpen(false)).some
              )
            else EmptyVdom
          )
