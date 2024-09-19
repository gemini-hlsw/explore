// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.users

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
import explore.model.UserInvitation
import explore.model.display.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.data.EmailAddress
import lucuma.core.data.EmailPred
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.core.syntax.display.*
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
import org.typelevel.log4cats.Logger
import queries.common.InvitationQueriesGQL.*
import queries.common.InvitationQueriesGQL.CreateInviteMutation.Data

case class InviteUserPopup(
  programId:          Program.Id,
  role:               ProgramUserRole,
  invitations:        View[List[UserInvitation]],
  createInviteStatus: View[CreateInviteStatus],
  overlayRef:         OverlayPanelRef
) extends ReactFnProps(InviteUserPopup.component)

object InviteUserPopup:
  private val MailValidator: InputValidSplitEpi[EmailAddress] =
    // Scala doesn't like type aliases with refined types?
    InputValidSplitEpi.refinedString[EmailPred].asInstanceOf[InputValidSplitEpi[EmailAddress]]

  private type Props = InviteUserPopup

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(none[EmailAddress])
      .useState(false)
      .useStateView(none[String])
      .render: (props, ctx, emailView, validEmail, key) =>
        import ctx.given

        val createInviteStatus: View[CreateInviteStatus] = props.createInviteStatus

        def createInvitation(
          email:   EmailAddress,
          viewKey: View[Option[String]]
        ): IO[Unit] =
          (props.createInviteStatus.set(CreateInviteStatus.Running).to[IO] *>
            CreateInviteMutation[IO].execute(
              props.programId,
              email.value.value,
              props.role
            )).attempt
            .flatMap:
              case Left(e)  =>
                Logger[IO].error(e)("Error creating invitation") *>
                  createInviteStatus.set(CreateInviteStatus.Error).to[IO]
              case Right(r) =>
                props.invitations.mod(r.createUserInvitation.invitation :: _).to[IO] *>
                  viewKey.set(r.createUserInvitation.key.some).to[IO] *>
                  createInviteStatus.set(CreateInviteStatus.Done).to[IO]

        OverlayPanel(
          closeOnEscape = true,
          onHide = key.set(None) >> emailView.set(None).runAsyncAndForget
        )(
          <.div(PrimeStyles.Dialog)(
            <.div(PrimeStyles.DialogHeader)(s"Create ${props.role.shortName} invitation"),
            <.div(PrimeStyles.DialogContent)(
              <.div(LucumaPrimeStyles.FormColumnCompact)(
                FormInputTextView(
                  id = "email-invite".refined,
                  value = emailView,
                  label = "Email",
                  disabled = createInviteStatus.get === CreateInviteStatus.Running,
                  validFormat = MailValidator.optional,
                  onValidChange = v => validEmail.setState(v)
                )(^.autoComplete := "off")
              ),
              <.div(LucumaPrimeStyles.FormColumn)(
                <.label(
                  "An invitation email has been sent. If you wish to send the invitation another way, copy and send the key below to the invited user, it won't be displayed again."
                )
              ).when(key.when(_.isDefined)),
              key.get.map(key =>
                <.div(LucumaPrimeStyles.FormColumn)(
                  CopyControl("Invite key", key)
                )
              )
            ),
            <.div(PrimeStyles.DialogFooter)(
              Message(
                text = "Error submitting user invite, try later",
                severity = Message.Severity.Error
              ).when(createInviteStatus.get === CreateInviteStatus.Error),
              Button(
                icon = Icons.Close,
                onClickE = e =>
                  createInviteStatus.set(CreateInviteStatus.Idle) *> props.overlayRef.toggle(e),
                label = "Close"
              ).compact.when(createInviteStatus.get === CreateInviteStatus.Done),
              Button(
                icon = Icons.PaperPlaneTop,
                loading = createInviteStatus.get === CreateInviteStatus.Running,
                disabled =
                  !validEmail.value || createInviteStatus.when(_ === CreateInviteStatus.Done),
                onClick = createInviteStatus.set(CreateInviteStatus.Idle) *>
                  emailView.get
                    .map: email =>
                      createInvitation(email, key).runAsync
                    .getOrEmpty,
                tooltip = "Send",
                label = "Invite"
              ).compact.when(createInviteStatus.get =!= CreateInviteStatus.Done)
            )
          )
        ).addModifiers(Seq(ExploreStyles.CompactOverlayPanel, ExploreStyles.InviteUserPopup))
          .withRef(props.overlayRef.ref)
