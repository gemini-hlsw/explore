// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.ResponseException
import clue.data.syntax.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import explore.*
import explore.DefaultErrorPolicy
import explore.Icons
import explore.common.ProposalQueries.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.CallForProposal
import explore.model.ProgramDetails
import explore.model.ProgramTimeRange
import explore.model.Proposal
import explore.model.ProposalAttachment
import explore.model.layout.LayoutsMap
import explore.syntax.ui.*
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramType
import lucuma.core.model.Program
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Image
import lucuma.react.primereact.Message
import lucuma.react.primereact.Tag
import lucuma.react.primereact.Toolbar
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.enums.ProposalStatus
import lucuma.ui.Resources
import lucuma.ui.components.LoginStyles
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import queries.common.ProposalQueriesGQL.*

case class ProposalTabContents(
  programId:         Program.Id,
  userVault:         Option[UserVault],
  programDetails:    View[ProgramDetails],
  cfps:              List[CallForProposal],
  timeEstimateRange: Pot[Option[ProgramTimeRange]],
  attachments:       View[List[ProposalAttachment]],
  undoStacks:        View[UndoStacks[IO, Proposal]],
  layout:            LayoutsMap
) extends ReactFnProps(ProposalTabContents.component)

object ProposalTabContents:
  private type Props = ProposalTabContents

  private object IsUpdatingStatus extends NewType[Boolean]

  private def createProposal(
    programId:      Program.Id,
    programDetails: View[ProgramDetails]
  )(using FetchClient[IO, ObservationDB], Logger[IO], ToastCtx[IO]): Callback =
    val proposal = Proposal.Default
    programDetails.zoom(ProgramDetails.proposal).set(proposal.some) >>
      CreateProposalMutation[IO]
        .execute(
          CreateProposalInput(
            programId = programId,
            SET = proposal.toInput
          )
        )
        .toastErrors
        .void
        .runAsync

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useStateView(IsUpdatingStatus(false))
    .useMemoBy((props, _, _) => props.programDetails.get.proposalStatus): (_, _, _) =>
      p => p === ProposalStatus.Submitted || p === ProposalStatus.Accepted
    .useState(none[String]) // Submission error message
    .useLayoutEffectWithDepsBy((props, _, _, _, _) =>
      props.programDetails.get.proposal.flatMap(_.callId)
    )((_, _, _, _, e) => _ => e.setState(none))
    .render: (props, ctx, isUpdatingStatus, readonly, errorMessage) =>

      import ctx.given

      val invitations = props.programDetails.zoom(ProgramDetails.invitations)
      val users       = props.programDetails.zoom(ProgramDetails.allUsers)

      val isStdUser      = props.userVault.map(_.user).collect { case _: StandardUser => () }.isDefined
      val proposalStatus = props.programDetails.get.proposalStatus

      def updateStatus(newStatus: ProposalStatus): Callback =
        (for {
          _ <- SetProposalStatus[IO]
                 .execute:
                   SetProposalStatusInput(programId = props.programId.assign, status = newStatus)
                 .onError:
                   case ResponseException(errors, _) =>
                     errorMessage.setState(errors.head.message.some).to[IO]
                   case e                            =>
                     errorMessage.setState(Some(e.getMessage.toString)).to[IO]
                 .void
          _ <- props.programDetails.zoom(ProgramDetails.proposalStatus).set(newStatus).toAsync
        } yield ()).switching(isUpdatingStatus.async, IsUpdatingStatus(_)).runAsync

      if (props.programDetails.get.programType =!= ProgramType.Science)
        <.div(ExploreStyles.HVCenter)(
          Message(
            text = "Only Science Program Types can have proposals.",
            severity = Message.Severity.Info
          )
        )
      else
        props.programDetails
          .zoom(ProgramDetails.proposal)
          .mapValue((proposalView: View[Proposal]) =>
            val piPartner =
              props.programDetails.zoom(ProgramDetails.piPartner.some).get

            val deadline: Option[Timestamp] =
              proposalView.get.deadline(props.cfps, piPartner)

            <.div(
              ExploreStyles.ProposalTab,
              ProposalEditor(
                props.programId,
                props.userVault.map(_.user.id),
                proposalView,
                props.undoStacks,
                props.timeEstimateRange,
                users,
                invitations,
                props.attachments,
                props.userVault.map(_.token),
                props.cfps,
                props.layout,
                readonly
              ),
              Toolbar(left =
                <.div(
                  ExploreStyles.ProposalSubmissionBar,
                  Tag(
                    value = props.programDetails.get.proposalStatus.name,
                    severity =
                      if (proposalStatus === ProposalStatus.Accepted) Tag.Severity.Success
                      else Tag.Severity.Danger
                  )
                    .when(proposalStatus > ProposalStatus.Submitted),
                  // TODO: Validate proposal before allowing submission
                  React
                    .Fragment(
                      Button(
                        label = "Submit Proposal",
                        onClick = updateStatus(ProposalStatus.Submitted),
                        disabled = isUpdatingStatus.get.value || proposalView.get.callId.isEmpty
                      ).compact.tiny,
                      deadline.map(CallDeadline.apply)
                    )
                    .when:
                      isStdUser && proposalStatus === ProposalStatus.NotSubmitted
                  ,
                  Button(
                    "Retract Proposal",
                    severity = Button.Severity.Warning,
                    onClick = updateStatus(ProposalStatus.NotSubmitted),
                    disabled = isUpdatingStatus.get.value
                  ).compact.tiny
                    .when:
                      isStdUser && proposalStatus === ProposalStatus.Submitted
                  ,
                  errorMessage.value.map(r => Message(text = r, severity = Message.Severity.Error))
                )
              )
            )
          )
          .getOrElse(
            if (isStdUser)
              <.div(ExploreStyles.HVCenter)(
                Button(
                  label = "Create a Proposal",
                  icon = Icons.FileCirclePlus.withClass(LoginStyles.LoginOrcidIcon),
                  clazz = LoginStyles.LoginBoxButton,
                  severity = Button.Severity.Secondary,
                  onClick = createProposal(
                    props.programId,
                    props.programDetails
                  )
                ).big
              )
            else
              <.div(ExploreStyles.HVCenter)(
                Button(
                  label = "Login with ORCID to create a Proposal",
                  icon = Image(src = Resources.OrcidLogo, clazz = LoginStyles.LoginOrcidIcon),
                  clazz = LoginStyles.LoginBoxButton,
                  severity = Button.Severity.Secondary,
                  onClick = ctx.sso.switchToORCID.runAsync
                ).big
              )
          )
