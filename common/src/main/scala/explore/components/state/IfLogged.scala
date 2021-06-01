// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all._
import clue.data.Input
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.common.UserPreferencesQueriesGQL._
import explore.components.UserSelectionForm
import explore.implicits._
import explore.model.RootModel
import explore.model.UserVault
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps
import explore.utils.reuse._
import explore.model.reusability._

final case class IfLogged(view: View[RootModel])(val render: (UserVault, IO[Unit]) ==> VdomNode)
    extends ReactProps[IfLogged](IfLogged.component)

object IfLogged {
  type Props = IfLogged

  // Creates a "profile" for user preferences.
  private def createUserPrefs(vault: UserVault)(implicit ctx: AppContextIO): IO[Unit] =
    UserInsertMutation.execute(Input(vault.user.id.toString)).start.void

  private val component =
    ScalaComponent
      .builder[IfLogged]
      .stateless
      .render_P { p =>
        AppCtx.using { implicit ctx =>
          val vaultView   = p.view.zoom(RootModel.vault)
          val messageView = p.view.zoom(RootModel.userSelectionMessage)

          val vaultSet   = vaultView.set.reuseAlways
          val messageSet = (messageView.set.compose((s: NonEmptyString) => s.some)).reuseAlways

          vaultView.get.fold[VdomElement](
            UserSelectionForm(vaultView, messageView)
          ) { vault =>
            React.Fragment(
              SSOManager(vault.expiration, vaultSet, messageSet),
              ConnectionManager(vault.token, onConnect = createUserPrefs(vault))(
                Reuse
                  .currying(vaultSet, messageSet, p.render.curry(vault))
                  .in((vaultSet, messageSet, render) => LogoutTracker(vaultSet, messageSet)(render))
              )
            )
          }
        }
      }
      .build
}
