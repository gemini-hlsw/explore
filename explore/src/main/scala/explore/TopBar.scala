// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import crystal.react.implicits._
import explore.common.SSOClient
import explore.components.ConnectionsStatus
import explore.components.ui.ExploreStyles
import explore.model.UserVault
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.reusability._
import react.common._
import react.semanticui.collections.menu._
import react.semanticui.modules.dropdown.Dropdown
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown.DropdownMenu

final case class TopBar(vault: View[UserVault]) extends ReactProps[TopBar](TopBar.component)

object TopBar {
  type Props = TopBar

  implicit val propsReuse = Reusability.derive[Props]

  private val component =
    ScalaComponent
      .builder[TopBar]
      .render_P { p =>
        <.div(
          ExploreStyles.MainHeader,
          Menu(
            attached = MenuAttached.Top,
            compact = true,
            borderless = true,
            tabular = MenuTabular.Right
          )(
            MenuItem(as = "a")(
              <.span(
                ExploreStyles.MainTitle,
                "Explore"
              )
            ),
            MenuMenu(position = MenuMenuPosition.Right)(
              MenuItem(as = "a", header = true)(
                <.span(
                  ExploreStyles.LoginMenu,
                  p.vault.zoom(UserVault.user).get.displayName
                ),
                ConnectionsStatus()
              ),
              Dropdown(item = true, simple = true, icon = Icons.UserCircle)(
                DropdownMenu(
                  DropdownItem(text = "Login/Register",
                               onClick = SSOClient.redirectToLogin[IO].runAsyncCB
                  )
                )
              )
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

}
