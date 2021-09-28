// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.effect.SyncIO
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse.Reuse
import explore.Icons
import explore.Resources
import explore.components.About
import explore.components.ConnectionsStatus
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.enum.ExecutionEnvironment
import explore.model.enum.Theme
import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.GuestRole
import lucuma.core.model.User
import lucuma.ui.reusability._
import monocle.Focus
import org.scalajs.dom
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

final case class TopBar(
  user:     User,
  onLogout: IO[Unit]
) extends ReactProps[TopBar](TopBar.component)

object TopBar {
  type Props = TopBar

  protected case class State(copied: Boolean = false, theme: Theme) {
    def flip: State =
      if (theme === Theme.Dark) copy(theme = Theme.Light) else copy(theme = Theme.Dark)
  }

  object State {
    val copied = Focus[State](_.copied)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.user)
  implicit val stateReuse: Reusability[State] = Reusability.derive

  def currentTheme: Theme =
    if (dom.document.body.classList.contains(Theme.Light.clazz.htmlClass))
      Theme.Light
    else
      Theme.Dark

  private val component =
    ScalaComponent
      .builder[TopBar]
      .initialState(State(false, currentTheme))
      .render { $ =>
        val p            = $.props
        val currentTheme = $.state.theme

        AppCtx.using { implicit appCtx =>
          val role = p.user.role

          def logout: IO[Unit] =
            appCtx.sso.logout >> p.onLogout

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
                  p.user.displayName
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
                        Reuse.always(
                          <.span(ExploreStyles.Version,
                                 ExploreStyles.VersionUncopied.when(! $.state.copied)
                          )(
                            s"Version: ${appCtx.version}",
                            CopyToClipboard(
                              text = appCtx.version.value,
                              onCopy = (_, copied) =>
                                $.setStateL(State.copied)(copied) >>
                                  $.setStateL(State.copied)(false).delayMs(1500).toCallback
                            )(
                              <.span(
                                Icons.Clipboard.when(! $.state.copied),
                                Icons.ClipboardCheck.when($.state.copied)
                              )
                            )
                          )
                        )
                      ),
                      DropdownDivider(),
                      DropdownItem(
                        onClick = appCtx.sso.switchToORCID.runAsyncCB
                      )(
                        <.div(ExploreStyles.OrcidMenu)(
                          Image(clazz = ExploreStyles.OrcidIconMenu, src = Resources.OrcidLogo),
                          <.span(^.cls := "text", "Switch to ORCID")
                        )
                      ).when(role === GuestRole),
                      DropdownItem(text = "Logout",
                                   icon = Icons.Logout.fixedWidth(),
                                   onClick = logout.runAsyncCB
                      ),
                      DropdownItem(onClick =
                        utils
                          .setupScheme[SyncIO](
                            if (currentTheme === Theme.Dark) Theme.Light else Theme.Dark
                          ) *> $.modStateIn[SyncIO](_.flip)
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
      .configure(Reusability.shouldComponentUpdate)
      .build

}
