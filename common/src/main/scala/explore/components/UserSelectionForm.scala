// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.Resources
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.UserVault
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewType
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.UAParser
import org.scalajs.dom
import react.common.*
import react.primereact.Button
import react.primereact.Dialog
import react.primereact.Message

case class UserSelectionForm(
  vault:   View[Option[UserVault]],
  message: View[Option[NonEmptyString]]
) extends ReactFnProps[UserSelectionForm](UserSelectionForm.component)

object UserSelectionForm:
  private type Props = UserSelectionForm

  private object IsOpen extends NewType[Boolean]
  private type IsOpen = IsOpen.Type

  private case class BrowserInfo(supportedOrcidBrowser: Boolean, warnBrowser: Boolean):
    @inline def showButtons: Boolean = supportedOrcidBrowser

  private object BrowserInfo:
    def supportedOrcidBrowser: IO[BrowserInfo] = IO {
      val browser  = UAParser(dom.window.navigator.userAgent).getBrowser()
      val verRegex = raw"(\d{0,3}).(\d{0,3})\.?(.*)?".r

      (browser.name, browser.version) match {
        case ("Safari", verRegex(major, _, _)) if major.toInt <= 13 => BrowserInfo(false, false)
        case ("Safari", _)                                          => BrowserInfo(true, true)
        case _                                                      => BrowserInfo(true, false)
      }
    }.handleError(_ => BrowserInfo(true, true))

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(IsOpen(true))
      .useEffectResultOnMount(BrowserInfo.supportedOrcidBrowser)
      .render((props, ctx, isOpen, browserInfoPot) =>
        import ctx.given

        val guest: Callback = ctx.sso.guest.flatMap(v => props.vault.set(v.some).to[IO]).runAsync

        val login: Callback = ctx.sso.redirectToLogin.runAsync

        Dialog(
          closable = false,
          visible = isOpen.get.value,
          onHide = isOpen.set(IsOpen(false)),
          resizable = false,
          draggable = false,
          showHeader = false,
          clazz = ExploreStyles.Dialog.Small
        )(
          browserInfoPot.render(browserInfo =>
            React.Fragment(
              <.div(
                ExploreStyles.LoginBoxLayout,
                Logo(),
                <.div(
                  ExploreStyles.UserSelectionButtons,
                  Button(
                    clazz = ExploreStyles.LoginBoxButton,
                    severity = Button.Severity.Secondary,
                    onClick = login >> props.message.set(none) >> isOpen.set(IsOpen(false))
                  ).big(
                    <.div(
                      ExploreStyles.LoginOrcidButton,
                      <.img(ExploreStyles.OrcidIcon, ^.src := Resources.OrcidLogo),
                      "Login with ORCID"
                    )
                  ).when(browserInfo.showButtons),
                  Button(
                    clazz = ExploreStyles.LoginBoxButton,
                    severity = Button.Severity.Secondary,
                    onClick = guest >> props.message.set(none) >> isOpen.set(IsOpen(false))
                  ).big(
                    <.div(
                      ExploreStyles.LoginOrcidButton,
                      Icons.UserAstronaut.withClass(ExploreStyles.OrcidIcon),
                      "Continue as Guest"
                    )
                  ).when(browserInfo.showButtons)
                )
              ),
              <.div(ExploreStyles.LoginMessagesLayout)(
                props.message.get.whenDefined(message =>
                  Message(text = message.value, severity = Message.Severity.Error)
                ),
                Message(
                  text =
                    "This version of Safari isn't supported. Try a newer version (≥14.0.1) or a recent version of Chrome or Firefox.",
                  severity = Message.Severity.Error,
                  icon = Icons.SkullCrossBones
                ).unless(browserInfo.supportedOrcidBrowser),
                Message(
                  text =
                    "ORCID authentication does not work with some configurations of Safari and MacOS. If it doesn't work for you please try Chrome or Firefox.",
                  severity = Message.Severity.Warning,
                  icon = Icons.ExclamationTriangle
                ).when(browserInfo.warnBrowser)
              )
            )
          )
        )
      )
