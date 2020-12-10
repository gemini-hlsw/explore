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
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.GuestRole
import lucuma.core.model.User
import lucuma.ui.reusability._
import react.common._
import react.semanticui.collections.menu._
import react.semanticui.elements.image.Image
import react.semanticui.modules.dropdown.Dropdown
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown.DropdownMenu
import react.semanticui.views.item.Item

final case class TopBar(
  user:   User,
  logout: IO[Unit]
) extends ReactProps[TopBar](TopBar.component)

object TopBar {
  type Props = TopBar

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.user)

  private val component =
    ScalaComponent
      .builder[TopBar]
      .render_P { p =>
        AppCtx.withCtx { implicit appCtx =>
          implicit val cs     = appCtx.cs
          implicit val logger = appCtx.logger
          val role            = p.user.role

          def logout: IO[Unit] =
            SSOClient.logout[IO](appCtx.ssoURI, IO.fromFuture) >> p.logout

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
                    DropdownItem(
                      onClick = SSOClient.switchToORCID[IO](appCtx.ssoURI, IO.fromFuture).runAsyncCB
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
