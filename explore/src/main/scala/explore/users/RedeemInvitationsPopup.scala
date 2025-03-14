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
import explore.model.Focused
import explore.model.IsActive
import explore.model.RedeemInvitationResult
import explore.model.enums.AppTab
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.UserInvitation
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.*
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import queries.common.InvitationQueriesGQL.RedeemInvitationMutation

enum RedeemInviteProcess(private val tag: String) derives Enumerated:
  case Idle    extends RedeemInviteProcess("idle")
  case Running extends RedeemInviteProcess("running")
  case Error   extends RedeemInviteProcess("error")
  case Done    extends RedeemInviteProcess("done")

case class RedeemInvitationsPopup(vault: UserVault, onClose: Option[Callback] = none)
    extends ReactFnProps(RedeemInvitationsPopup.Component)

object RedeemInvitationsPopup:
  private object IsOpen     extends NewType[Boolean]
  private object ErrorMsg   extends NewType[Option[String]]
  private object IsKeyValid extends NewType[Boolean]

  private val Component = ScalaFnComponent[RedeemInvitationsPopup]: props =>
    for {
      ctx        <- useContext(AppContext.ctx)
      isOpen     <- useState(IsOpen(true))
      active     <- useStateView[IsActive](IsActive(false))
      process    <- useStateView(RedeemInviteProcess.Idle)
      key        <- useStateView("")
      isKeyValid <- useStateView(IsKeyValid(false))
      errorMsg   <- useStateView(ErrorMsg(none))
      result     <- useStateView(none[RedeemInvitationResult])
    } yield
      import ctx.given

      val onHide = props.onClose.map(oc => isOpen.setState(IsOpen(false)) >> oc)

      // tolerate leading and trailing spaces from copy/paste
      def validateKey(key: String): Callback =
        isKeyValid.set(IsKeyValid(UserInvitation.fromString.getOption(key.trim).isDefined))

      def redeem(key: String): IO[Unit] =
        RedeemInvitationMutation[IO]
          .execute(key)
          .raiseGraphQLErrors
          .flatMap(l =>
            (process.set(RedeemInviteProcess.Done) *>
              errorMsg.set(ErrorMsg(none)) *>
              result.set(l.redeemUserInvitation.invitation.some))
              .to[IO]
          )
          .handleErrorWith {
            case ResponseException(errors, _) =>
              val msg = errors
                .foldMap(_.message)

              (process.set(RedeemInviteProcess.Error) *>
                errorMsg.set(ErrorMsg(msg.some)))
                .to[IO]
            case e                            =>
              val msg = s"Error redeeming invitation with key $key"
              (process.set(RedeemInviteProcess.Error) *> errorMsg.set(ErrorMsg(msg.some))).to[IO]
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
          disabled = !isKeyValid.get.value || process.get === RedeemInviteProcess.Done,
          onClick = process.set(RedeemInviteProcess.Running) *> redeem(key.get.trim).runAsync,
          label = "Redeem"
        ).compact.unless(process.get === RedeemInviteProcess.Done),
        result.get.map(r =>
          Button(
            icon = Icons.ArrowUp,
            onClick = isOpen.setState(IsOpen(false)) *>
              ctx.setPageVia((AppTab.Program, r.pid, Focused.None).some, SetRouteVia.HistoryPush),
            label = s"Open program ${r.pid}"
          ).compact
        ),
        Button(
          icon = Icons.Close,
          onClick = isOpen.setState(IsOpen(false)),
          label = "Close"
        ).compact.when(process.get === RedeemInviteProcess.Done)
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
        header = "Redeem invitation.",
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
              onTextChange = validateKey,
              disabled = process.get === RedeemInviteProcess.Running
            )(^.autoComplete := "off")
          )
        )
      )
