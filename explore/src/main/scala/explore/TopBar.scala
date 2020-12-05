// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import explore.Icons
import explore.WebpackResources
import explore.common.SSOClient
import explore.components.ConnectionsStatus
import explore.components.ui.ExploreStyles
import explore.model.UserVault
import explore.model.reusability._
import explore.utils.ExploreEvent
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.GuestRole
import lucuma.ui.reusability._
import org.scalajs.dom.window
import react.common._
import react.semanticui.collections.menu._
import react.semanticui.elements.image.Image
import react.semanticui.modules.dropdown.Dropdown
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown.DropdownMenu
import react.semanticui.views.item.Item

final case class TopBar(vault: View[UserVault]) extends ReactProps[TopBar](TopBar.component)

object TopBar {
  type Props = TopBar

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.vault)

  private val component =
    ScalaComponent
      .builder[TopBar]
      .render_P { p =>
        val ssoURI = p.vault.get.ssoURI
        AppCtx.withCtx { implicit appCtx =>
          implicit val cs     = appCtx.cs
          implicit val logger = appCtx.logger
          val user            = p.vault.zoom(UserVault.user).get
          val role            = user.role

          def logout: IO[Unit] =
            SSOClient.logout[IO](ssoURI, IO.fromFuture) *>
              IO(
                appCtx.bc.postMessage(ExploreEvent.Logout)
              ).attempt *>
              // Let's just reload rather than trying to reset the state
              IO(window.location.reload())

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
                user.displayName
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
                    DropdownItem(
                      onClick = SSOClient.switchToORCID[IO](ssoURI, IO.fromFuture).runAsyncCB
                    )(
                      <.div(ExploreStyles.OrcidMenu)(
                        Image(clazz = ExploreStyles.OrcidIconMenu,
                              src = WebpackResources.OrcidLogo
                        ),
                        <.span(^.cls := "text", "Switch to ORCID")
                      )
                    ).when(role === GuestRole),
                    DropdownItem(text = "Logout", icon = Icons.Logout, onClick = logout.runAsyncCB)
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
