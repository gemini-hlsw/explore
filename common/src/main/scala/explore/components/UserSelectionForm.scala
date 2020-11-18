// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import cats.implicits._
import cats.effect.concurrent.Deferred
import explore.model.UserVault
import react.semanticui.elements.button.Button
import org.scalajs.dom
import cats.effect.Sync
import explore.common.SSOClient.FromFuture
import explore.common.SSOClient
import crystal.react.implicits._
import cats.effect.Effect
import sttp.client3.Response
import cats.FlatMap
import react.semanticui.modules.modal.Modal
import react.semanticui.modules.modal.ModalSize
import react.semanticui.modules.modal.ModalContent
import explore.components.ui.ExploreStyles
import explore.Icons
import react.semanticui.elements.image.Image
import scalajs.js
import scalajs.js.annotation._
import react.semanticui.sizes.Big

final case class UserSelectionForm[F[_]: FlatMap: Effect](
  complete:   Deferred[F, UserVault],
  fromFuture: FromFuture[F, Response[Either[String, String]]]
) extends ReactProps[UserSelectionForm[Any]](UserSelectionForm.component) {
  def guest: Callback = SSOClient.guest(fromFuture).flatMap(complete.complete(_)).runAsyncCB
  def login: Callback = SSOClient.redirectToLogin[F].runAsyncCB
}

object UserSelectionForm {
  type Props[F[_]] = UserSelectionForm[F]

  // Explicitly never reuse as we don't much care about flushing this one
  implicit def propsReuse[F[_]]: Reusability[UserSelectionForm[F]] = Reusability.never

  @js.native
  @JSImport("resources/images/ORCID-iD_icon-vector.svg", JSImport.Default)
  val orcidLogo: String = js.native

  val component =
    ScalaComponent
      .builder[Props[Any]]("UserSelectionForm")
      .stateless
      .render_P { p =>
        Modal(
          size = ModalSize.Large,
          clazz = ExploreStyles.LoginBox,
          content = ModalContent(
            <.div(
              ExploreStyles.LoginBoxLayout,
              <.div(ExploreStyles.LoginTitleWrapper)(
                <.div(ExploreStyles.LoginTitle, "Explore")
              ),
              Button(
                content = <.div(ExploreStyles.LoginOrcidButton,
                                Image(clazz = ExploreStyles.OrcidIcon, src = orcidLogo),
                                "Login with ORCID"
                ),
                clazz = ExploreStyles.LoginBoxButton,
                size = Big,
                onClick = p.login
              ),
              Button(content = "Continue as Guest",
                     size = Big,
                     clazz = ExploreStyles.LoginBoxButton,
                     onClick = p.guest,
                     icon = Icons.UserAstronaut
              )
            )
          ),
          open = true
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

  def launch[F[_]: Sync: Effect: FlatMap](
    d:          Deferred[F, UserVault],
    fromFuture: FromFuture[F, Response[Either[String, String]]]
  ): F[Unit] = Sync[F].delay {
    val container = Option(dom.document.getElementById("root")).getOrElse {
      val elem = dom.document.createElement("div")
      elem.id = "root"
      dom.document.body.appendChild(elem)
      elem
    }

    UserSelectionForm(d, fromFuture).renderIntoDOM(container)
    ()

  }
}
