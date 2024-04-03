// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.users

import cats.effect.IO
import cats.syntax.all.*
import clue.ResponseException
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.IsActive
import explore.model.ProgramInvitation
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.*
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import queries.common.InvitationQueriesGQL.RedeemInvitationMutation

enum RedeemInviteProcess(private val tag: String) derives Enumerated:
  case Idle    extends RedeemInviteProcess("idle")
  case Running extends RedeemInviteProcess("running")
  case Error   extends RedeemInviteProcess("error")
  case Done    extends RedeemInviteProcess("done")

case class RedeemInvitationsPopup(vault: UserVault, onClose: Option[Callback] = none)
    extends ReactFnProps(RedeemInvitationsPopup.component)

object RedeemInvitationsPopup:
  private type Props = RedeemInvitationsPopup

  private object IsOpen   extends NewType[Boolean]
  private object ErrorMsg extends NewType[Option[String]]

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useState(IsOpen(true))
    .useStateView[IsActive](IsActive(false))
    .useStateView(RedeemInviteProcess.Idle)
    .useStateView("")
    .useStateView(ErrorMsg(none))
    .useStateView(none[ProgramInvitation])
    .render: (props, ctx, isOpen, active, process, key, errorMsg, result) =>
      import ctx.given

      val onHide = props.onClose.map(oc => isOpen.setState(IsOpen(false)) >> oc)

      def redeem(key: String): IO[Unit] =
        RedeemInvitationMutation[IO]
          .execute(key)
          .flatMap(l => result.set(l.redeemUserInvitation.invitation.some).to[IO])
          .handleErrorWith {
            case ResponseException(errors, _) =>
              val msg = errors.foldMap(_.message)
              errorMsg.set(ErrorMsg(msg.some)).to[IO]
            case e                            =>
              val msg = s"Error redeeming invitation with key $key"
              Logger[IO].error(e)(msg) *> errorMsg.set(ErrorMsg(msg.some)).to[IO]
          }
          .void

      val footer = <.div(
        errorMsg.get.value.map(e => Message(text = e, severity = Message.Severity.Error)),
        result.get.map(r =>
          Message(text = s"Granted acceess to program ${r.pid} by ${r.givenName} ${r.familyName}",
                  severity = Message.Severity.Success
          )
        ),
        Button(
          icon = Icons.PaperPlaneTop,
          loading = process.get === RedeemInviteProcess.Running,
          disabled = key.get.isEmpty || process.get === RedeemInviteProcess.Done,
          onClick = redeem(key.get).runAsync,
          label = "Redeem"
        ).compact
      )

      Dialog(
        visible = isOpen.value.value,
        onHide = onHide.orEmpty,
        position = DialogPosition.Top,
        closeOnEscape = props.onClose.isDefined,
        closable = props.onClose.isDefined,
        dismissableMask = props.onClose.isDefined,
        resizable = false,
        clazz = LucumaPrimeStyles.Dialog.Small |+| ExploreStyles.ApiKeysPopup,
        header = "Reedem invitation.",
        footer = footer
      )(
        React.Fragment(
          Divider(),
          <.label("To accept an invitation you should have a key of the form xxxx.xxxxxxxx...."),
          <.div(LucumaPrimeStyles.FormColumn)(
            FormInputTextView(
              id = "email-invite".refined,
              value = key,
              label = "Key:",
              disabled = process.get === RedeemInviteProcess.Running
            )(^.autoComplete := "off")
          )
        )
      )
