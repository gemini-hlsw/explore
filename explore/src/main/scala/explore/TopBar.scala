// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.Reuse
import crystal.react.reuse.*
import explore.Icons
import explore.Resources
import explore.components.About
import explore.components.ConnectionsStatus
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ExploreLocalPreferences
import explore.model.ExploreLocalPreferences.*
import explore.model.ModelUndoStacks
import explore.model.enums.ExecutionEnvironment
import explore.programs.ProgramsPopup
import explore.syntax.ui.*
import explore.syntax.ui.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.callback.CallbackCatsEffect.*
import japgolly.scalajs.react.vdom.html_<^.*
import log4cats.loglevel.LogLevelLogger
import lucuma.core.model.GuestRole
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.ui.enums.Theme
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom
import org.scalajs.dom.window
import react.common.ReactFnProps
import react.semanticui.collections.menu.*
import react.semanticui.elements.image.Image
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.dropdown.Dropdown
import react.semanticui.modules.dropdown.DropdownDivider
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown.DropdownMenu
import react.semanticui.shorthand.*
import react.semanticui.views.item.Item
import typings.loglevel.mod.LogLevelDesc

case class TopBar(
  user:        User,
  programId:   Option[Program.Id],
  preferences: ExploreLocalPreferences,
  undoStacks:  View[ModelUndoStacks[IO]],
  onLogout:    IO[Unit]
) extends ReactFnProps(TopBar.component)

object TopBar:
  private type Props = TopBar

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useState(false) // isProgramsOpen
      .useState(false) // just to force rerenders
      .useEffectResultWithDepsBy((_, _, _, toggle) => toggle.value)((_, _, _, _) =>
        _ => Theme.current
      )
      .render { (props, ctx, isProgramsOpen, toggle, themePot) =>
        import ctx.given

        val role = props.user.role

        def logout: IO[Unit] = ctx.sso.logout >> props.onLogout

        val level = props.preferences.level

        def setLogLevel(l: LogLevelDesc): Callback =
          (ExploreLocalPreferences
            .storePreferences[IO](
              props.preferences.copy(level = l)
            ) *> IO(window.location.reload(false))).runAsync

        <.div(
          ExploreStyles.MainHeader,
          Menu(
            attached = MenuAttached.Top,
            borderless = true,
            tabular = MenuTabular.Right
          )(
            MenuItem(
              <.span(
                ExploreStyles.MainTitle,
                "Explore"
              )
            ),
            Item(
              ExploreStyles.MainUserName,
              props.user.displayName
            ),
            ConnectionsStatus(),
            MenuMenu(position = MenuMenuPosition.Right, clazz = ExploreStyles.MainMenu)(
              Dropdown(
                item = true,
                simple = true,
                compact = true,
                icon = Icons.Bars,
                open = false,
                clazz = ExploreStyles.MainMenuDropdown
              )(
                DropdownMenu(
                  About(
                    Reuse.always(
                      DropdownItem(text = "About Explore", icon = Icons.Info.withFixedWidth())
                    )
                  ),
                  DropdownItem(
                    text = "Manage Programs",
                    icon = Icons.ListCheck.withFixedWidth(),
                    onClick = isProgramsOpen.setState(true)
                  ),
                  TagMod.when(isProgramsOpen.value)(
                    ProgramsPopup(
                      props.programId,
                      props.undoStacks,
                      isProgramsOpen.setState(false).some.reuseAlways
                    )
                  ),
                  DropdownDivider(),
                  DropdownItem(
                    onClick = ctx.sso.switchToORCID.runAsync
                  )(
                    <.div(ExploreStyles.OrcidMenu)(
                      Image(clazz = ExploreStyles.OrcidIconMenu, src = Resources.OrcidLogo),
                      <.span(^.cls := "text", "Login with ORCID")
                    )
                  ).when(role === GuestRole),
                  DropdownItem(
                    text = "Logout",
                    icon = Icons.Logout.withFixedWidth(),
                    onClick = logout.runAsync
                  ),
                  DropdownItem()(
                    Icons.BarCodeRead.withFixedWidth(),
                    "Log Level",
                    DropdownMenu(
                      DropdownItem(onClick = setLogLevel(LogLevelDesc.INFO))(
                        Checkbox(label = "Info", checked = level =!= LogLevelDesc.DEBUG)
                      ),
                      DropdownItem(onClick = setLogLevel(LogLevelDesc.DEBUG))(
                        Checkbox(label = "Debug", checked = level === LogLevelLogger.Level.DEBUG)
                      )
                    )
                  ).when(ctx.environment =!= ExecutionEnvironment.Production),
                  themePot
                    .map(currentTheme =>
                      DropdownItem(
                        onClick =
                          (if (currentTheme === Theme.Light) Theme.Dark.setup[CallbackTo]
                           else Theme.Light.setup[CallbackTo]) >> toggle.setState(!toggle.value)
                      )(
                        Checkbox(label = "Dark/Light", checked = currentTheme === Theme.Dark)
                      ).when(ctx.environment === ExecutionEnvironment.Development)
                    )
                    .toOption
                    .whenDefined,
                  DropdownItem(
                    text = "Toggle Reusability",
                    icon = Icons.CrystalBall.withFixedWidth(),
                    onClick = utils.toggleReusabilityOverlay[CallbackTo]()
                  )
                    .when(ctx.environment === ExecutionEnvironment.Development)
                )
              )
            )
          )
        )
      }
