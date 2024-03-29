// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import explore.*
import explore.DefaultErrorPolicy
import explore.Icons
import explore.common.ProgramQueries
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ProgramDetails
import explore.model.ProgramTimeRange
import explore.model.ProposalAttachment
import explore.model.layout.LayoutsMap
import explore.syntax.ui.*
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Proposal
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Image
import lucuma.react.primereact.Tag
import lucuma.react.primereact.Toolbar
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.enums.ProposalStatus
import lucuma.schemas.odb.input.*
import lucuma.ui.Resources
import lucuma.ui.components.LoginStyles
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import queries.common.ProgramQueriesGQL.*

case class ProposalTabContents(
  programId:         Program.Id,
  userVault:         Option[UserVault],
  programDetails:    View[ProgramDetails],
  timeEstimateRange: Pot[Option[ProgramTimeRange]],
  attachments:       View[List[ProposalAttachment]],
  undoStacks:        View[UndoStacks[IO, Proposal]],
  layout:            LayoutsMap
) extends ReactFnProps(ProposalTabContents.component)

object ProposalTabContents:
  private object IsUpdatingStatus extends NewType[Boolean]
  private type IsUpdatingStatus = IsUpdatingStatus.Type

  private def createProposal(
    programId:      Program.Id,
    programDetails: View[ProgramDetails]
  )(using FetchClient[IO, ObservationDB], Logger[IO], ToastCtx[IO]): Callback =
    val proposal = Proposal.Default
    programDetails.zoom(ProgramDetails.proposal).set(proposal.some) >>
      UpdateProgramsMutation[IO]
        .execute(
          UpdateProgramsInput(
            WHERE = programId.toWhereProgram.assign,
            SET = ProgramPropertiesInput(proposal = proposal.toInput.assign)
          )
        )
        .toastErrors
        .void
        .runAsync

  private def renderFn(
    programId:         Program.Id,
    userVault:         Option[UserVault],
    programDetails:    View[ProgramDetails],
    timeEstimateRange: Pot[Option[ProgramTimeRange]],
    attachments:       View[List[ProposalAttachment]],
    undoStacks:        View[UndoStacks[IO, Proposal]],
    ctx:               AppContext[IO],
    layout:            LayoutsMap,
    isUpdatingStatus:  View[IsUpdatingStatus],
    readonly:          Boolean
  ): VdomNode = {
    import ctx.given

    val details = programDetails.get
    val users   = details.allUsers

    val isStdUser      = userVault.map(_.user).collect { case _: StandardUser => () }.isDefined
    val proposalStatus = programDetails.get.proposalStatus

    def updateStatus(newStatus: ProposalStatus): Callback =
      (for {
        _ <- ProgramQueries.updateProposalStatus[IO](programId, newStatus)
        _ <- programDetails.zoom(ProgramDetails.proposalStatus).set(newStatus).toAsync
      } yield ()).switching(isUpdatingStatus.async, IsUpdatingStatus(_)).runAsync

    programDetails
      .zoom(ProgramDetails.proposal)
      .mapValue((proposalView: View[Proposal]) =>
        <.div(
          ExploreStyles.ProposalTab,
          ProposalEditor(
            programId,
            userVault.map(_.user.id),
            proposalView,
            undoStacks,
            timeEstimateRange,
            users,
            attachments,
            userVault.map(_.token),
            layout,
            readonly
          ),
          Toolbar(left =
            <.span(
              Tag(
                value = programDetails.get.proposalStatus.name,
                severity =
                  if (proposalStatus === ProposalStatus.Accepted) Tag.Severity.Success
                  else Tag.Severity.Danger
              )
                .when(proposalStatus > ProposalStatus.Submitted),
              // TODO: Validate proposal before allowing submission
              Button(label = "Submit Proposal",
                     onClick = updateStatus(ProposalStatus.Submitted),
                     disabled = isUpdatingStatus.get.value
              ).compact.tiny
                .when(
                  isStdUser && proposalStatus === ProposalStatus.NotSubmitted
                ),
              Button("Retract Proposal",
                     severity = Button.Severity.Warning,
                     onClick = updateStatus(ProposalStatus.NotSubmitted),
                     disabled = isUpdatingStatus.get.value
              ).compact.tiny
                .when(
                  isStdUser && proposalStatus === ProposalStatus.Submitted
                )
            )
          )
        )
      )
      .getOrElse(
        if (isStdUser)
          <.div(
            ExploreStyles.HVCenter,
            Button(
              label = "Create a Proposal",
              icon = Icons.FileCirclePlus.withClass(LoginStyles.LoginOrcidIcon),
              clazz = LoginStyles.LoginBoxButton,
              severity = Button.Severity.Secondary,
              onClick = createProposal(
                programId,
                programDetails
              )
            ).big
          )
        else
          <.div(
            ExploreStyles.HVCenter,
            Button(
              label = "Login with ORCID to create a Proposal",
              icon = Image(src = Resources.OrcidLogo, clazz = LoginStyles.LoginOrcidIcon),
              clazz = LoginStyles.LoginBoxButton,
              severity = Button.Severity.Secondary,
              onClick = ctx.sso.switchToORCID.runAsync
            ).big
          )
      )
  }

  private type Props = ProposalTabContents

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useStateView(IsUpdatingStatus(false))
    .useMemoBy((props, _, _) => props.programDetails.get.proposalStatus)((_, _, _) =>
      _ === ProposalStatus.Submitted
    )
    .render { (props, ctx, isUpdatingStatus, readonly) =>
      renderFn(
        props.programId,
        props.userVault,
        props.programDetails,
        props.timeEstimateRange,
        props.attachments,
        props.undoStacks,
        ctx,
        props.layout,
        isUpdatingStatus,
        readonly
      )
    }
