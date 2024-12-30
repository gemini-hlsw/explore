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
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.ProgramQueries
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ExploreModelValidators
import explore.model.ProgramUser
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.data.EmailAddress
import lucuma.core.data.EmailPred
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
  programUser:        View[ProgramUser],
  createInviteStatus: View[CreateInviteStatus],
  overlayRef:         OverlayPanelRef
) extends ReactFnProps(InviteUserPopup.Component):
  val programUserId = programUser.get.id

object InviteUserPopup:
  private val Component = ScalaFnComponent[InviteUserPopup](props =>
    for {
      ctx        <- useContext(AppContext.ctx)
      emailView  <- useStateView(none[EmailAddress])
      validEmail <- useState(false)
      key        <- useStateView(none[String])
      _          <- useEffectWithDeps(props.programUser.get.email): propsEmail =>
                      val asEmailAddress = propsEmail.flatMap(EmailAddress.from(_).toOption)
                      emailView.set(asEmailAddress) >>
                        validEmail.setState(asEmailAddress.isDefined)
    } yield {
      import ctx.given

      val createInviteStatus: View[CreateInviteStatus] = props.createInviteStatus

      def createInvitation(
        email:   EmailAddress,
        viewKey: View[Option[String]]
      ): IO[Unit] =
        (createInviteStatus.set(CreateInviteStatus.Running).to[IO] *>
          CreateInviteMutation[IO].execute(
            props.programUserId,
            email.value.value
          )).attempt
          .flatMap:
            case Left(e)  =>
              Logger[IO].error(e)("Error creating invitation") *>
                createInviteStatus.set(CreateInviteStatus.Error).to[IO]
            case Right(r) =>
              // set the fallback email address of the user to this email if it is
              // different from the calculated email address of the user.
              val setEmail: IO[Unit] =
                if (props.programUser.get.email.exists(_ === email.value.value)) IO.unit
                else
                  props.programUser
                    .zoom(ProgramUser.fallbackEmail)
                    .set(email.some)
                    .to[IO] >>
                    ProgramQueries
                      .updateUserFallbackEmail(props.programUserId, email.value.value.some)

              props.programUser
                .zoom(ProgramUser.invitations)
                .mod(r.createUserInvitation.invitation :: _)
                .to[IO] *>
                setEmail *>
                viewKey.set(r.createUserInvitation.key.some).to[IO] *>
                createInviteStatus.set(CreateInviteStatus.Done).to[IO]

      OverlayPanel(
        closeOnEscape = true,
        onHide = key.set(None) >> emailView.set(None).runAsyncAndForget
      )(
        <.div(PrimeStyles.Dialog)(
          <.div(PrimeStyles.DialogHeader)(s"Create invitation"),
          <.div(PrimeStyles.DialogContent)(
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              FormInputTextView(
                id = "email-invite".refined,
                value = emailView,
                label = "Email",
                disabled = createInviteStatus.get === CreateInviteStatus.Running,
                validFormat = ExploreModelValidators.MailValidator.optional,
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
              onClickE =
                e => createInviteStatus.set(CreateInviteStatus.Idle) *> props.overlayRef.toggle(e),
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
    }
  )
