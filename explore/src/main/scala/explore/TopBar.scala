// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import explore.components.ui.ExploreStyles
import explore.components.{ About, ConnectionsStatus }
import explore.model.enum.{ ExecutionEnvironment, Theme }
import explore.{ Icons, WebpackResources }
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.{ GuestRole, User }
import lucuma.ui.reusability._
import monocle.macros.Lenses
import org.scalajs.dom
import react.clipboard.CopyToClipboard
import react.common._
import react.semanticui.collections.menu._
import react.semanticui.elements.image.Image
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.dropdown.{ Dropdown, DropdownDivider, DropdownItem, DropdownMenu }
import react.semanticui.views.item.Item

final case class TopBar(
  user:   User,
  logout: IO[Unit]
) extends ReactProps[TopBar](TopBar.component)

object TopBar {
  type Props = TopBar

  @Lenses
  protected case class State(copied: Boolean = false, theme: Theme) {
    def flip: State =
      if (theme === Theme.Dark) copy(theme = Theme.Light) else copy(theme = Theme.Dark)
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

        AppCtx.withCtx { implicit appCtx =>
          implicit val cs = appCtx.cs
          val role        = p.user.role

          def logout: IO[Unit] =
            appCtx.sso.logout >> p.logout

          React.Fragment(
            <.div(
              ExploreStyles.MainHeader,
              Menu(
                attached = MenuAttached.Top,
                // compact = true,
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
                           clazz = ExploreStyles.MainMenuDropdown
                  )(
                    DropdownMenu(
                      About(
                        DropdownItem(text = "About Explore", icon = Icons.Info),
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
                      ),
                      DropdownDivider(),
                      DropdownItem(
                        onClick = appCtx.sso.switchToORCID.runAsyncCB
                      )(
                        <.div(ExploreStyles.OrcidMenu)(
                          Image(clazz = ExploreStyles.OrcidIconMenu,
                                src = WebpackResources.OrcidLogo
                          ),
                          <.span(^.cls := "text", "Switch to ORCID")
                        )
                      ).when(role === GuestRole),
                      DropdownItem(text = "Logout",
                                   icon = Icons.Logout,
                                   onClick = logout.runAsyncCB
                      ),
                      DropdownItem(onClick =
                        utils
                          .setupScheme[IO](
                            if (currentTheme === Theme.Dark) Theme.Light else Theme.Dark
                          )
                          .runAsyncCB *> $.modState(_.flip)
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
