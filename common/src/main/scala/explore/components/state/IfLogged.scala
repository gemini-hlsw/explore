// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import clue.data.Input
import crystal.react.View
import crystal.react.reuse.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.UserSelectionForm
import explore.model.AppContext
import explore.model.RootModel
import explore.model.UserVault
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.UserPreferencesDB
import react.common.*

case class IfLogged(view: View[RootModel])(
  val render:             (UserVault, IO[Unit]) => VdomNode
) extends ReactFnProps(IfLogged.component)

object IfLogged:
  private type Props = IfLogged

  // Creates a "profile" for user preferences.
  private def createUserPrefs(vault: UserVault)(using
    TransactionalClient[IO, UserPreferencesDB]
  ): IO[Unit] =
    UserInsertMutation.execute(Input(vault.user.id.toString)).start.void

  private val component =
    ScalaFnComponent
      .withHooks[IfLogged]
      .useContext(AppContext.ctx)
      .render { (props, ctx) =>
        import ctx.given

        val vaultView   = props.view.zoom(RootModel.vault)
        val messageView = props.view.zoom(RootModel.userSelectionMessage)

        val vaultSet   = vaultView.set.reuseAlways
        val messageSet = messageView.set.compose((s: NonEmptyString) => s.some).reuseAlways

        vaultView.get.fold[VdomElement](
          UserSelectionForm(vaultView, messageView)
        ) { vault =>
          React.Fragment(
            SSOManager(vault.expiration, vaultSet, messageSet),
            ConnectionManager(vault.token, onConnect = vault.curryReusing.in(createUserPrefs _))(
              LogoutTracker(vaultSet, messageSet)(props.render(vault, _))
            )
          )
        }
      }
