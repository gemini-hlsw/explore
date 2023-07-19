// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.*
import crystal.react.hooks.*
import explore.components.About
import explore.components.ConnectionsStatus
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ExploreLocalPreferences
import explore.model.ExploreLocalPreferences.*
import explore.model.ProgramInfoList
import explore.model.ProgramSummaries
import explore.model.enums.ExecutionEnvironment
import explore.programs.ProgramsPopup
import explore.undo.UndoStacks
import explore.users.UserPreferencesPopup
import japgolly.scalajs.react.*
import japgolly.scalajs.react.callback.CallbackCatsEffect.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.GuestRole
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.ui.enums.Theme
import lucuma.ui.reusability.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import org.scalajs.dom
import org.scalajs.dom.window
import react.common.*
import react.fa.FontAwesomeIcon
import react.primereact.Button
import react.primereact.Image
import react.primereact.MenuItem
import react.primereact.PopupTieredMenu
import react.primereact.Toolbar
import react.primereact.hooks.all.*
import typings.loglevel.mod.LogLevelDesc

case class TopBar(
  vault:        View[UserVault],
  programId:    Option[Program.Id],
  preferences:  ExploreLocalPreferences,
  undoStacks:   View[UndoStacks[IO, ProgramSummaries]],
  programInfos: ViewOpt[ProgramInfoList],
  onLogout:     IO[Unit]
) extends ReactFnProps(TopBar.component)

object TopBar:
  private type Props = TopBar

  private object IsAboutOpen extends NewType[Boolean]

  private object IsProgramOpen extends NewType[Boolean]

  private object IsUserPropertiesOpen extends NewType[Boolean]

  private type ForceRerender = ForceRerender.Type
  private object ForceRerender extends NewType[Boolean]:
    extension (s: ForceRerender)
      def flip: ForceRerender =
        if (s.value) ForceRerender(true) else ForceRerender(false)

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useState(IsProgramOpen(false))
      .useStateView(IsAboutOpen(false))
      .useState(ForceRerender(false))
      .useState(IsUserPropertiesOpen(false))
      .usePopupMenuRef
      .useEffectResultWithDepsBy((_, _, _, _, toggle, _, _) => toggle.value)(
        (_, _, _, _, _, _, _) => _ => Theme.current
      )
      .render {
        (
          props,
          ctx,
          isProgramsOpen,
          isAboutOpen,
          toggle,
          isUserPropertiesOpen,
          menuRef,
          themePot
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
          val themeMenuItem                          = themePot
            .map(currentTheme =>
              MenuItem.SubMenu(label = "Theme",
                               icon = Icons.Eclipse,
                               visible = ctx.environment === ExecutionEnvironment.Development
              )(
                MenuItem.Item(
                  label = "Dark",
                  icon = Icons.Moon,
                  disabled = currentTheme === Theme.Dark,
                  command = Theme.Dark.setup[CallbackTo] >> toggle.modState(_.flip)
                ),
                MenuItem.Item(
                  label = "Light",
                  icon = Icons.SunBright,
                  disabled = currentTheme === Theme.Light,
                  command = Theme.Light.setup[CallbackTo] >> toggle.modState(_.flip)
                )
              )
            )
            .toOption
            .getOrElse(MenuItem.Item(label = "placeholder", visible = false))

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
            MenuItem.Separator,
            MenuItem.Item(
              label = "Login with ORCID",
              icon = Image(src = Resources.OrcidLogo, clazz = ExploreStyles.OrcidIconMenu),
              visible = role === GuestRole,
              command = ctx.sso.switchToORCID.runAsync
            ),
            MenuItem.Item(label = "Logout", icon = Icons.Logout, command = logout.runAsync),
            MenuItem.SubMenu(
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
              )
            ),
            themeMenuItem,
            MenuItem.Item(
              label = "Toggle Reusability",
              icon = Icons.CrystalBall,
              command = utils.toggleReusabilityOverlay[CallbackTo](),
              visible = ctx.environment === ExecutionEnvironment.Development
            )
          )

          val menuItems =
            if (role =!= GuestRole) {
              firstItems :::
                List(
                  MenuItem
                    .Item(
                      label = "User Preferences",
                      icon = Icons.UserGears,
                      command = isUserPropertiesOpen.setState(IsUserPropertiesOpen(true))
                    )
                ) ::: lastItems
            } else firstItems ::: lastItems

          React.Fragment(
            Toolbar(
              clazz = ExploreStyles.MainHeader,
              left = <.span(ExploreStyles.MainTitle, "Explore"),
              right = React.Fragment(
                RoleSwitch(props.vault),
                ConnectionsStatus(),
                Button(icon = Icons.Bars,
                       text = true,
                       severity = Button.Severity.Secondary,
                       onClickE = menuRef.toggle
                )
              )
            ),
            PopupTieredMenu(model = menuItems).withRef(menuRef.ref),
            if (isAboutOpen.get.value) About(isAboutOpen.zoom(IsAboutOpen.value))
            else EmptyVdom,
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
                isUserPropertiesOpen.setState(IsUserPropertiesOpen(false)).some
              )
            else EmptyVdom
          )
      }
