// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.Icons
import explore.Resources
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.UserVault
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.utils.UAParser
import org.scalajs.dom
import react.common._
import react.common.implicits._
import react.semanticui.elements.button.Button
import react.semanticui.elements.image.Image
import react.semanticui.elements.label.Label
import react.semanticui.modules.modal.Modal
import react.semanticui.modules.modal.ModalContent
import react.semanticui.modules.modal.ModalSize
import react.semanticui.sizes._

final case class UserSelectionForm(
  vault:   View[Option[UserVault]],
  message: View[Option[NonEmptyString]]
) extends ReactProps[UserSelectionForm](UserSelectionForm.component) {
  def guest(implicit ctx: AppContextIO): Callback =
    ctx.sso.guest.flatMap(v => vault.set(v.some)).runAsyncCB
  def login(implicit ctx: AppContextIO): Callback =
    ctx.sso.redirectToLogin.runAsyncCB

  def supportedOrcidBrowser: CallbackTo[(Boolean, Boolean)] = CallbackTo[(Boolean, Boolean)] {
    val browser  = new UAParser(dom.window.navigator.userAgent).getBrowser()
    val verRegex = raw"(\d{0,3}).(\d{0,3})\.?(.*)?".r
    (browser.name, browser.version) match {
      case ("Safari", verRegex(major, _, _)) if major.toInt <= 13 => (false, false)
      case ("Safari", _)                                          => (true, true)
      case _                                                      => (true, false)
    }
  }.handleError(_ => CallbackTo.pure((true, true)))
}

object UserSelectionForm {
  type Props = UserSelectionForm

  final case class State(supportedOrcidBrowser: Boolean, warnBrowser: Boolean) {
    val showButtons: Boolean = supportedOrcidBrowser
  }

  // Explicitly never reuse as we don't much care about flushing this one
  implicit val propsReuse: Reusability[UserSelectionForm] = Reusability.never
  implicit var stateReuse: Reusability[State]             = Reusability.derive

  val component =
    ScalaComponent
      .builder[Props]
      .initialStateCallbackFromProps { p =>
        p.supportedOrcidBrowser.map(Function.tupled(State.apply _))
      }
      .render_PS { (p, s) =>
        AppCtx.using { implicit ctx =>
          Modal(
            size = ModalSize.Large,
            clazz = ExploreStyles.LoginBox,
            content = ModalContent(
              <.div(
                ExploreStyles.LoginBoxLayout,
                Logo(),
                Button(
                  content = <.div(ExploreStyles.LoginOrcidButton,
                                  Image(clazz = ExploreStyles.OrcidIcon, src = Resources.OrcidLogo),
                                  "Login with ORCID"
                  ),
                  clazz = ExploreStyles.LoginBoxButton,
                  size = Big,
                  onClick = p.login >> p.message.set(none).runAsyncCB
                ).when(s.showButtons),
                Button(content = "Continue as Guest",
                       size = Big,
                       clazz = ExploreStyles.LoginBoxButton,
                       onClick = p.guest >> p.message.set(none).runAsyncCB,
                       icon = Icons.UserAstronaut
                ).when(s.showButtons),
                p.message.get.whenDefined(message =>
                  Label(size = Large,
                        clazz = ExploreStyles.LoginBoxButton |+| ExploreStyles.ErrorLabel
                  )(message.value)
                ),
                Label(size = Large,
                      clazz = ExploreStyles.LoginBoxButton |+| ExploreStyles.ErrorLabel
                )(
                  Icons.SkullCrossBones.size(Big),
                  "This version of Safari isn't supported. Try a newer version (≥14.0.1) or a recent version of Chrome or Firefox."
                ).unless(s.supportedOrcidBrowser),
                Label(size = Large,
                      clazz = ExploreStyles.LoginBoxButton |+| ExploreStyles.WarningLabel
                )(
                  Icons.WarningSign.size(Big),
                  "ORCID authentication does not work with some configurations of Safari and MacOS. If it doesn't work for you please try Chrome or Firefox."
                ).when(s.warnBrowser)
              )
            ),
            open = true
          )
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
