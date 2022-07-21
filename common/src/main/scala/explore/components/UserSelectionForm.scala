// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.Resources
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.UserVault
import explore.utils._
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
import react.semanticui.shorthand._
import react.semanticui.sizes._

final case class UserSelectionForm(
  vault:        View[Option[UserVault]],
  message:      View[Option[NonEmptyString]]
)(implicit ctx: AppContextIO)
    extends ReactFnProps[UserSelectionForm](UserSelectionForm.component) {
  val guest: Callback =
    ctx.sso.guest.flatMap(v => vault.set(v.some).to[IO]).runAsync

  val login: Callback =
    ctx.sso.redirectToLogin.runAsync
}

object UserSelectionForm {
  protected type Props = UserSelectionForm

  final private case class BrowserInfo(supportedOrcidBrowser: Boolean, warnBrowser: Boolean) {
    @inline def showButtons: Boolean = supportedOrcidBrowser
  }

  private object BrowserInfo {
    def supportedOrcidBrowser: IO[BrowserInfo] = IO {
      val browser  = UAParser(dom.window.navigator.userAgent).getBrowser()
      val verRegex = raw"(\d{0,3}).(\d{0,3})\.?(.*)?".r

      (browser.name, browser.version) match {
        case ("Safari", verRegex(major, _, _)) if major.toInt <= 13 => BrowserInfo(false, false)
        case ("Safari", _)                                          => BrowserInfo(true, true)
        case _                                                      => BrowserInfo(true, false)
      }
    }.handleError(_ => BrowserInfo(true, true))
  }

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useEffectResultOnMount(BrowserInfo.supportedOrcidBrowser)
      .render((props, browserInfoPot) =>
        Modal(
          size = ModalSize.Large,
          clazz = ExploreStyles.LoginBox,
          content = ModalContent(
            browserInfoPot.render(browserInfo =>
              <.div(
                ExploreStyles.LoginBoxLayout,
                Logo(),
                <.div(
                  ExploreStyles.UserSelectionButtons,
                  Button(
                    content = <.div(
                      ExploreStyles.LoginOrcidButton,
                      Image(clazz = ExploreStyles.OrcidIcon, src = Resources.OrcidLogo),
                      "Login with ORCID"
                    ),
                    clazz = ExploreStyles.LoginBoxButton,
                    size = Big,
                    onClick = props.login >> props.message.set(none)
                  ).when(browserInfo.showButtons),
                  Button(
                    content = <.div(
                      ExploreStyles.LoginOrcidButton,
                      Icons.UserAstronaut
                        .clazz(ExploreStyles.OrcidIcon),
                      "Continue as Guest"
                    ),
                    size = Big,
                    clazz = ExploreStyles.LoginBoxButton,
                    onClick = props.guest >> props.message.set(none)
                  ).when(browserInfo.showButtons)
                ),
                props.message.get.whenDefined(message =>
                  Label(
                    size = Large,
                    clazz = ExploreStyles.LoginBoxButton |+| ExploreStyles.ErrorLabel
                  )(message.value)
                ),
                Label(
                  size = Large,
                  clazz = ExploreStyles.LoginBoxButton |+| ExploreStyles.ErrorLabel
                )(
                  Icons.SkullCrossBones,
                  "This version of Safari isn't supported. Try a newer version (≥14.0.1) or a recent version of Chrome or Firefox."
                ).unless(browserInfo.supportedOrcidBrowser),
                Label(size = Large,
                      clazz = ExploreStyles.LoginBoxButton |+| ExploreStyles.WarningLabel
                )(
                  Icons.ExclamationTriangle,
                  "ORCID authentication does not work with some configurations of Safari and MacOS. If it doesn't work for you please try Chrome or Firefox."
                ).when(browserInfo.warnBrowser)
              )
            )
          ),
          open = true
        )
      )
}
