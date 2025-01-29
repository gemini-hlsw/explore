// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.*
import crystal.react.*
import explore.*
import explore.Icons
import explore.common.ProposalQueries.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.AttachmentList
import explore.model.CallForProposal
import explore.model.ProgramDetails
import explore.model.ProgramTimeRange
import explore.model.ProgramUser
import explore.model.Proposal
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
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Image
import lucuma.react.primereact.Message
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.enums.ProposalStatus
import lucuma.ui.LucumaStyles
import lucuma.ui.Resources
import lucuma.ui.components.LoginStyles
import lucuma.ui.primereact.*
import lucuma.ui.sso.UserVault
import org.typelevel.log4cats.Logger
import queries.common.ProposalQueriesGQL.*

case class ProposalTabContents(
  programId:            Program.Id,
  userVault:            Option[UserVault],
  programDetails:       View[ProgramDetails],
  cfps:                 List[CallForProposal],
  timeEstimateRange:    Pot[Option[ProgramTimeRange]],
  attachments:          View[AttachmentList],
  undoStacks:           View[UndoStacks[IO, ProgramDetails]],
  layout:               LayoutsMap,
  userIsReadonlyCoi:    Boolean,
  hasProposalObsErrors: Boolean
) extends ReactFnProps(ProposalTabContents.component)

object ProposalTabContents:
  private type Props = ProposalTabContents

  private def createProposal(
    programId:      Program.Id,
    programDetails: View[ProgramDetails]
  )(using FetchClient[IO, ObservationDB], Logger[IO], ToastCtx[IO]): Callback =
    val proposal = Proposal.Default
    programDetails.zoom(ProgramDetails.proposal).set(proposal.some) >>
      CreateProposalMutation[IO]
        .execute:
          CreateProposalInput(
            programId = programId,
            SET = proposal.toInput
          )
        .raiseGraphQLErrors
        .toastErrors
        .void
        .runAsync

  private val component = ScalaFnComponent[Props]: props =>
    for {
      ctx <- useContext(AppContext.ctx)
    } yield
      import ctx.given

      val proposalStatus     = props.programDetails.get.proposalStatus
      val proposalIsReadonly =
        proposalStatus === ProposalStatus.Submitted || proposalStatus === ProposalStatus.Accepted

      val undoCtx: UndoContext[ProgramDetails] =
        UndoContext(props.undoStacks, props.programDetails)

      val users: View[List[ProgramUser]] =
        props.programDetails.zoom(ProgramDetails.allUsers)

      val isStdUser: Boolean =
        props.userVault.map(_.user).collect { case StandardUser(_, _, _, _) => () }.isDefined

      if (props.programDetails.get.programType =!= ProgramType.Science)
        <.div(LucumaStyles.HVCenter)(
          Message(
            text = "Only Science Program Types can have proposals.",
            severity = Message.Severity.Info
          )
        )
      else
        undoCtx
          .zoom(ProgramDetails.proposal.some)
          .map((proposal: UndoSetter[Proposal]) =>
            val piPartner =
              props.programDetails.zoom(ProgramDetails.piPartner.some).get

            val deadline: Option[Timestamp] =
              proposal.get.deadline(props.cfps, piPartner)

            <.div(ExploreStyles.ProposalTab)(
              ProposalEditor(
                props.programId,
                props.userVault,
                undoCtx,
                proposal,
                props.timeEstimateRange,
                users,
                props.attachments,
                props.userVault.map(_.token),
                props.cfps,
                props.layout,
                proposalIsReadonly,
                props.userIsReadonlyCoi
              ),
              ProposalSubmissionBar(
                props.programId,
                props.programDetails.zoom(ProgramDetails.proposalStatus),
                deadline,
                proposal.get.callId,
                isStdUser && !props.userIsReadonlyCoi,
                props.hasProposalObsErrors
              )
            )
          )
          .getOrElse(
            if (isStdUser)
              <.div(LucumaStyles.HVCenter)(
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
              <.div(LucumaStyles.HVCenter)(
                Button(
                  label = "Login with ORCID to create a Proposal",
                  icon = Image(src = Resources.OrcidLogo, clazz = LoginStyles.LoginOrcidIcon),
                  clazz = LoginStyles.LoginBoxButton,
                  severity = Button.Severity.Secondary,
                  onClick = ctx.sso.switchToORCID.runAsync
                ).big
              )
          )
