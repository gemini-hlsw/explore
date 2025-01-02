// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.*
import crystal.react.*
import explore.*
import explore.DefaultErrorPolicy
import explore.Icons
import explore.common.ProposalQueries.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.AttachmentList
import explore.model.CallForProposal
import explore.model.ObsSiteAndTargets
import explore.model.ProgramDetails
import explore.model.ProgramTimeRange
import explore.model.ProgramUser
import explore.model.Proposal
import explore.model.TargetList
import explore.model.layout.LayoutsMap
import explore.syntax.ui.*
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramType
import lucuma.core.math.Coordinates
import lucuma.core.model.CallCoordinatesLimits
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.ObjectTracking
import lucuma.core.model.Program
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.util.DateInterval
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
import lucuma.ui.reusability.given
import lucuma.ui.sso.UserVault
import org.typelevel.log4cats.Logger
import queries.common.ProposalQueriesGQL.*

import java.time.Instant

case class ProposalTabContents(
  programId:         Program.Id,
  userVault:         Option[UserVault],
  programDetails:    View[ProgramDetails],
  cfps:              List[CallForProposal],
  timeEstimateRange: Pot[Option[ProgramTimeRange]],
  attachments:       View[AttachmentList],
  obsTargets:        ObsSiteAndTargets,
  undoStacks:        View[UndoStacks[IO, Proposal]],
  layout:            LayoutsMap,
  userIsReadonlyCoi: Boolean
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
        .execute(
          CreateProposalInput(
            programId = programId,
            SET = proposal.toInput
          )
        )
        .toastErrors
        .void
        .runAsync

  private val component = ScalaFnComponent[Props]: props =>
    for {
      ctx      <- useContext(AppContext.ctx)
      readonly <-
        useMemo((props.programDetails.get.proposalStatus, props.userIsReadonlyCoi)):
          (status, roCoi) =>
            status === ProposalStatus.Submitted || status === ProposalStatus.Accepted || roCoi
    } yield
      import ctx.given

      val users: View[List[ProgramUser]] =
        props.programDetails.zoom(ProgramDetails.allUsers)

      val isStdUser: Boolean =
        props.userVault.map(_.user).collect { case StandardUser(_, _, _, _) => () }.isDefined

      def coordinatesAt(asterism: TargetList, when: Instant): Option[CoordinatesAtVizTime] =
        for
          ast      <- NonEmptyList.fromList(asterism.values.toList)
          tracking  = ObjectTracking.fromAsterism(ast)
          coordsAt <- tracking.at(when)
        yield coordsAt

      if (props.programDetails.get.programType =!= ProgramType.Science)
        <.div(LucumaStyles.HVCenter)(
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

            val interval: Option[DateInterval] =
              props.cfps
                .find(c => proposalView.get.callId.exists(_ === c.id))
                .map(_.active)

            val deadline: Option[Timestamp] =
              proposalView.get.deadline(props.cfps, piPartner)

            val limits: Option[CallCoordinatesLimits] =
              props.cfps
                .find(c => proposalView.get.callId.exists(_ === c.id))
                .map(_.coordinateLimits)

            val outOfLimitsTargets = props.obsTargets.count { case (_, (site, tl)) =>
              val midpoint = interval.map(site.midpoint)
              midpoint.flatMap(coordinatesAt(tl, _)).exists { c =>
                !limits.exists(_.siteLimits(site).inLimits(c.value))
              }
            }

            <.div(ExploreStyles.ProposalTab)(
              ProposalEditor(
                props.programId,
                props.userVault.map(_.user.id),
                proposalView,
                props.undoStacks,
                props.timeEstimateRange,
                users,
                props.attachments,
                props.userVault.map(_.token),
                props.cfps,
                props.layout,
                readonly
              ),
              ProposalSubmissionBar(
                props.programId,
                props.programDetails.zoom(ProgramDetails.proposalStatus),
                deadline,
                proposalView.get.callId,
                isStdUser && !props.userIsReadonlyCoi || outOfLimitsTargets > 0
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
