// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.FlatMap
import cats.effect.Effect
import cats.effect.Sync
import cats.effect.concurrent.Deferred
import cats.implicits._
import crystal.react.implicits._
import explore.Icons
import explore.WebpackResources
import explore.common.SSOClient
import explore.common.SSOClient.FromFuture
import explore.components.ui.ExploreStyles
import explore.model.UserVault
import explore.utils.UAParser
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import react.common._
import react.semanticui.elements.button.Button
import react.semanticui.elements.image.Image
import react.semanticui.modules.modal.Modal
import react.semanticui.modules.modal.ModalContent
import react.semanticui.modules.modal.ModalSize
import react.semanticui.sizes._
import sttp.client3.Response
import sttp.model.Uri
import react.semanticui.elements.label.Label

final case class UserSelectionForm[F[_]: Effect](
  ssoURI:     Uri,
  complete:   Deferred[F, UserVault],
  fromFuture: FromFuture[F, Response[Either[String, String]]]
) extends ReactProps[UserSelectionForm[Any]](UserSelectionForm.component) {
  def guest: Callback = SSOClient.guest(ssoURI, fromFuture).flatMap(complete.complete(_)).runAsyncCB
  def login: Callback = SSOClient.redirectToLogin[F](ssoURI).runAsyncCB

  def supportedOrcidBrowser: CallbackTo[Boolean] = CallbackTo[Boolean] {
    val browser  = new UAParser(dom.window.navigator.userAgent).getBrowser()
    val verRegex = raw"(\d{0,3}).(\d{0,3}).*".r
    (browser.name, browser.version) match {
      case ("Safari", verRegex(major, minor)) if major.toInt <= 13 && minor.toInt < 1 => false
      case _                                                                          => true
    }
  }.handleError(_ => CallbackTo.pure(true))

}

object UserSelectionForm {
  type Props[F[_]] = UserSelectionForm[F]

  // Explicitly never reuse as we don't much care about flushing this one
  implicit def propsReuse[F[_]]: Reusability[UserSelectionForm[F]] = Reusability.never

  val component =
    ScalaComponent
      .builder[Props[Any]]("UserSelectionForm")
      .initialStateCallbackFromProps { p =>
        p.supportedOrcidBrowser
      }
      .render_PS { (p, s) =>
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
                content =
                  <.div(ExploreStyles.LoginOrcidButton,
                        Image(clazz = ExploreStyles.OrcidIcon, src = WebpackResources.OrcidLogo),
                        "Login with ORCID"
                  ),
                clazz = ExploreStyles.LoginBoxButton,
                size = Big,
                onClick = p.login
              ).when(s),
              Label(size = Large, clazz = ExploreStyles.LoginBoxButton)(
                Icons.SadTear.size(Big),
                "This browser isn't supported, recent versions of Chrome or Firefox are recommended"
              ).unless(s),
              Button(content = "Continue as Guest",
                     size = Big,
                     clazz = ExploreStyles.LoginBoxButton,
                     onClick = p.guest,
                     icon = Icons.UserAstronaut
              ).when(s)
            )
          ),
          open = true
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

  def launch[F[_]: Sync: Effect: FlatMap](
    ssoURI:     Uri,
    d:          Deferred[F, UserVault],
    fromFuture: FromFuture[F, Response[Either[String, String]]]
  ): F[Unit] = Sync[F].delay {
    val container = Option(dom.document.getElementById("root")).getOrElse {
      val elem = dom.document.createElement("div")
      elem.id = "root"
      dom.document.body.appendChild(elem)
      elem
    }

    UserSelectionForm(ssoURI, d, fromFuture).renderIntoDOM(container)
    ()

  }
}
