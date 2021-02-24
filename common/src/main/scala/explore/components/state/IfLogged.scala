// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all._
import explore.components.UserSelectionForm
import explore.implicits._
import explore.model.RootModel
import explore.model.UserVault
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps

final case class IfLogged(view: View[RootModel])(val render: (UserVault, IO[Unit]) => VdomNode)
    extends ReactProps[IfLogged](IfLogged.component)

object IfLogged {
  type Props = IfLogged

  private val component =
    ScalaComponent
      .builder[IfLogged]
      .stateless
      .render_P { p =>
        val vaultView   = p.view.zoom(RootModel.vault)
        val messageView = p.view.zoom(RootModel.userSelectionMessage)

        vaultView.get.fold[VdomElement](
          UserSelectionForm(vaultView, messageView)
        ) { vault =>
          React.Fragment(
            SSOManager(vault.expiration, vaultView.set, messageView.set.compose(_.some)),
            ConnectionManager(vault.token),
            LogoutTracker(vaultView.set, messageView.set.compose(_.some))(onLogout =>
              p.render(vault, onLogout)
            )
          )
        }
      }
      .build
}
