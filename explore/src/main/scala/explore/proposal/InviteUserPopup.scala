// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.Eq
import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats.given
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.EmailPred
import explore.model.InvitationStatus
import explore.model.RefinedEmail
import explore.model.UserInvitation
import io.circe.Decoder
import io.circe.syntax.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.react.primereact.OverlayPanel
import lucuma.react.primereact.OverlayPanelRef
import lucuma.react.primereact.PrimeStyles
import lucuma.refined.*
import lucuma.ui.components.CopyControl
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import queries.common.InvitationQueriesGQL.*
import queries.common.InvitationQueriesGQL.CreateInviteMutation.Data

case class InviteUserPopup(
  pid:         Program.Id,
  ref:         OverlayPanelRef,
  invitations: View[List[UserInvitation]]
) extends ReactFnProps(InviteUserPopup.component)

object InviteUserPopup:
  val MailValidator: InputValidSplitEpi[RefinedEmail] =
    InputValidSplitEpi.refinedString[EmailPred]

  private type Props = InviteUserPopup

  // We can move this to the query once the model is moved to lucuma-core
  extension (d: Data)
    // This is unsafe because we are assuming the server will always return a valid status
    def unsafeUserInvitation: UserInvitation =
      UserInvitation(
        d.createUserInvitation.invitation.id,
        Refined.unsafeApply[String, EmailPred](d.createUserInvitation.invitation.recipientEmail),
        Decoder[InvitationStatus]
          .decodeJson(
            d.createUserInvitation.invitation.status.asJson
          )
          .getOrElse(InvitationStatus.Revoked)
      )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(CreateInviteProcess.Idle)
      .useStateView(none[RefinedEmail])
      .useState(false)
      .useStateView(none[String])
      .render: (props, ctx, inviteState, emailView, validEmail, key) =>
        import ctx.given
        def createInvitation(
          createInvite: View[CreateInviteProcess],
          pid:          Program.Id,
          email:        RefinedEmail,
          viewKey:      View[Option[String]]
        ): IO[Unit] =
          (createInvite.set(CreateInviteProcess.Running).to[IO] *>
            CreateInviteMutation[IO].execute(pid, email.value)).attempt
            .flatMap {
              case Left(e)  =>
                Logger[IO].error(e)("Error creating invitation") *>
                  createInvite.set(CreateInviteProcess.Error).to[IO]
              case Right(r) =>
                props.invitations
                  .mod(r.unsafeUserInvitation :: _)
                  .to[IO] *>
                  viewKey.set(r.createUserInvitation.key.some).to[IO] *>
                  createInvite.set(CreateInviteProcess.Done).to[IO]
            }

        OverlayPanel(closeOnEscape = true)(
          <.div(
            PrimeStyles.Dialog,
            <.div(PrimeStyles.DialogHeader, "Create CoI invitation"),
            <.div(PrimeStyles.DialogContent)(
              <.div(LucumaPrimeStyles.FormColumnCompact)(
                FormInputTextView(
                  id = "email-invite".refined,
                  value = emailView,
                  label = "email",
                  disabled = inviteState.get === CreateInviteProcess.Running,
                  validFormat = MailValidator.optional,
                  onValidChange = v => validEmail.setState(v)
                )(^.autoComplete := "off")
              ),
              <.div(LucumaPrimeStyles.FormColumn)(
                <.label(
                  "Invite ready, please send the link below to you collaborator, it won't be displayed again"
                )
              ).when(key.when(_.isDefined)),
              key.get.map(key =>
                <.div(LucumaPrimeStyles.FormColumn)(
                  CopyControl("Invite Link", dom.window.location.origin + s"/redeem-invite/${key}")
                )
              )
            ),
            <.div(PrimeStyles.DialogFooter)(
              <.div(
                Message(text = "Error submitting user invite, try later",
                        severity = Message.Severity.Error
                ).when(inviteState.get === CreateInviteProcess.Error),
                Button(
                  icon = Icons.PaperPlaneTop,
                  loading = inviteState.get === CreateInviteProcess.Running,
                  disabled = !validEmail.value || inviteState.get === CreateInviteProcess.Done,
                  onClick = emailView.get
                    .map(e => createInvitation(inviteState, props.pid, e, key).runAsync)
                    .getOrEmpty,
                  tooltip = "Send",
                  label = "Invite"
                ).compact
              )
            )
          )
        ).addModifiers(Seq(ExploreStyles.CompactOverlayPanel, ExploreStyles.InviteUserPopup))
          .withRef(props.ref.ref)
