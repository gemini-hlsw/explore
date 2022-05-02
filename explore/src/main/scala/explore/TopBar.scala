// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all._
import crystal.react.ReuseView
import crystal.react.implicits._
import crystal.react.reuse.Reuse
import explore.Icons
import explore.Resources
import explore.components.About
import explore.components.ConnectionsStatus
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ExploreLocalPreferences
import explore.model.ExploreLocalPreferences._
import explore.model.ModelUndoStacks
import explore.model.enum.ExecutionEnvironment
import explore.model.enum.Theme
import explore.programs.ProgramsPopup
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCatsEffect._
import japgolly.scalajs.react.vdom.html_<^._
import log4cats.loglevel.LogLevelLogger
import lucuma.core.model.GuestRole
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.ui.reusability._
import org.scalajs.dom
import org.scalajs.dom.window
import react.clipboard.CopyToClipboard
import react.common._
import react.semanticui.collections.menu._
import react.semanticui.elements.image.Image
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.dropdown.Dropdown
import react.semanticui.modules.dropdown.DropdownDivider
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown.DropdownMenu
import react.semanticui.shorthand._
import react.semanticui.views.item.Item
import typings.loglevel.mod.LogLevelDesc

final case class TopBar(
  user:        User,
  programId:   Option[Program.Id],
  preferences: ExploreLocalPreferences,
  undoStacks:  ReuseView[ModelUndoStacks[IO]],
  onLogout:    IO[Unit]
) extends ReactFnProps[TopBar](TopBar.component)

object TopBar {
  type Props = TopBar

  implicit val propsReuse: Reusability[Props] = Reusability.by(p => (p.user, p.programId))

  def currentTheme: Theme =
    if (dom.document.body.classList.contains(Theme.Light.clazz.htmlClass))
      Theme.Light
    else
      Theme.Dark

  def flipTheme(theme: Theme): Theme =
    if (theme === Theme.Dark) Theme.Light else Theme.Dark

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      // copied
      .useState(false)
      // theme
      .useState(currentTheme)
      .renderWithReuse { (props, copied, theme) =>
        AppCtx.using { implicit appCtx =>
          val role = props.user.role

          def logout: IO[Unit] =
            appCtx.sso.logout >> props.onLogout

          val level = props.preferences.level

          def setLogLevel(l: LogLevelDesc): Callback =
            (ExploreLocalPreferences
              .storePreferences[IO](
                props.preferences.copy(level = l)
              ) *> IO(window.location.reload(false))).runAsync

          React.Fragment(
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
                  Dropdown(item = true,
                           simple = true,
                           compact = true,
                           icon = Icons.Bars,
                           open = false,
                           clazz = ExploreStyles.MainMenuDropdown
                  )(
                    DropdownMenu(
                      About(
                        Reuse.always(
                          DropdownItem(text = "About Explore", icon = Icons.Info.fixedWidth())
                        ),
                        Reuse.never(
                          <.span(ExploreStyles.Version,
                                 ExploreStyles.VersionUncopied.when(!copied.value)
                          )(
                            s"Version: ${appCtx.version}",
                            CopyToClipboard(
                              text = appCtx.version.value,
                              onCopy = (_, copiedCallback) =>
                                copied.setState(copiedCallback) *>
                                  copied.setState(false).delayMs(1500).toCallback
                            )(
                              <.span(Icons.Clipboard.unless(copied.value),
                                     Icons.ClipboardCheck.when(copied.value)
                              )
                            )
                          )
                        )
                      ),
                      ProgramsPopup(
                        currentProgramId = props.programId,
                        undoStacks = props.undoStacks,
                        trigger = Reuse
                          .always((cb: Callback) =>
                            DropdownItem(
                              text = "Manage Programs",
                              icon = Icons.ListCheck.fixedWidth(),
                              onClick = cb
                            ): VdomNode
                          )
                          .some
                      ),
                      DropdownDivider(),
                      DropdownItem(
                        onClick = appCtx.sso.switchToORCID.runAsync
                      )(
                        <.div(ExploreStyles.OrcidMenu)(
                          Image(clazz = ExploreStyles.OrcidIconMenu, src = Resources.OrcidLogo),
                          <.span(^.cls := "text", "Switch to ORCID")
                        )
                      ).when(role === GuestRole),
                      DropdownItem(text = "Logout",
                                   icon = Icons.Logout.fixedWidth(),
                                   onClick = logout.runAsync
                      ),
                      DropdownItem()(
                        Icons.BarCodeRead.fixedWidth(),
                        "Log Level",
                        DropdownMenu(
                          DropdownItem(onClick = setLogLevel(LogLevelDesc.INFO))(
                            Checkbox(label = "Info", checked = level =!= LogLevelDesc.DEBUG)
                          ),
                          DropdownItem(onClick = setLogLevel(LogLevelDesc.DEBUG))(
                            Checkbox(label = "Debug",
                                     checked = level === LogLevelLogger.Level.DEBUG
                            )
                          )
                        )
                      ).when(appCtx.environment =!= ExecutionEnvironment.Production),
                      DropdownItem(onClick =
                        utils
                          .setupScheme[CallbackTo](
                            if (theme.value === Theme.Dark) Theme.Light else Theme.Dark
                          ) *> theme.modState(flipTheme)
                      )(
                        Checkbox(label = "Dark/Light", checked = currentTheme === Theme.Dark)
                      )
                        .when(appCtx.environment === ExecutionEnvironment.Development)
                    )
                  )
                )
              )
            )
          )
        }
      }

}
