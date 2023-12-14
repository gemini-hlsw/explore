// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.Eq
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import crystal.react.*
import crystal.react.hooks.*
import explore.DefaultErrorPolicy
import explore.Icons
import explore.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ProgramUser
import explore.model.ProgramUserWithRole
import explore.model.ProposalAttachment
import explore.syntax.ui.*
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Proposal
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Image
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.Resources
import lucuma.ui.components.LoginStyles
import lucuma.ui.primereact.*
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.effect.*
import lucuma.ui.syntax.pot.*
import monocle.Focus
import monocle.Lens
import org.typelevel.log4cats.Logger
import queries.common.ProgramQueriesGQL.*

case class ProposalTabContents(
  programId:   Program.Id,
  userVault:   Option[UserVault],
  attachments: View[List[ProposalAttachment]],
  undoStacks:  View[UndoStacks[IO, Proposal]]
) extends ReactFnProps(ProposalTabContents.component)

object ProposalTabContents:
  private def createProposal(
    programId:        Program.Id,
    optProposalView:  View[Option[ProposalInfo]],
    minExecutionTime: Option[TimeSpan],
    maxExecutionTime: Option[TimeSpan]
  )(using FetchClient[IO, ObservationDB], Logger[IO], ToastCtx[IO]): Callback =
    val proposal = Proposal.Default
    optProposalView.set(
      ProposalInfo(proposal.some, minExecutionTime, maxExecutionTime, none, Nil).some
    ) >>
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
    programId:   Program.Id,
    userVault:   Option[UserVault],
    attachments: View[List[ProposalAttachment]],
    undoStacks:  View[UndoStacks[IO, Proposal]],
    ctx:         AppContext[IO]
  )(optProposalInfo: View[Option[ProposalInfo]]): VdomNode =
    import ctx.given

    optProposalInfo
      .mapValue { (proposalInfo: View[ProposalInfo]) =>
        val minExecutionTime = proposalInfo.get.minExecutionTime
        val maxExecutionTime = proposalInfo.get.maxExecutionTime
        val users            = proposalInfo.get.allUsers

        proposalInfo
          .zoom(ProposalInfo.optProposal)
          .mapValue((proposalView: View[Proposal]) =>
            ProposalEditor(
              programId,
              proposalView,
              undoStacks,
              minExecutionTime,
              maxExecutionTime,
              users,
              attachments,
              userVault.map(_.token)
            ): VdomNode
          )
          .getOrElse(userVault.map(_.user) match {
            case Some(_: StandardUser) =>
              <.div(
                ExploreStyles.HVCenter,
                Button(
                  label = "Create a Proposal",
                  icon = Icons.FileCirclePlus.withClass(LoginStyles.LoginOrcidIcon),
                  clazz = LoginStyles.LoginBoxButton,
                  severity = Button.Severity.Secondary,
                  onClick =
                    createProposal(programId, optProposalInfo, minExecutionTime, maxExecutionTime)
                ).big
              )
            case _                     =>
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
          })
      }
      .getOrElse(<.div(s"Program $programId not found!"))

  private type Props = ProposalTabContents

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useStreamResourceViewOnMountBy { (props, ctx) =>
      import ctx.given

      ProgramProposalQuery[IO]
        .query(props.programId)
        .map(data =>
          data.program
            .map { prog =>
              ProposalInfo(prog.proposal,
                           prog.timeEstimateRange.map(_.minimum.total),
                           prog.timeEstimateRange.map(_.maximum.total),
                           prog.pi,
                           prog.users
              )
            }
        )
        .reRunOnResourceSignals(ProgramEditSubscription.subscribe[IO](props.programId.assign))
    }
    .render { (props, ctx, optPropInfo) =>
      optPropInfo.renderPotOption(
        renderFn(props.programId, props.userVault, props.attachments, props.undoStacks, ctx) _
      )
    }

case class ProposalInfo(
  optProposal:      Option[Proposal],
  minExecutionTime: Option[TimeSpan],
  maxExecutionTime: Option[TimeSpan],
  pi:               Option[ProgramUser],
  programUsers:     List[ProgramUserWithRole]
) derives Eq:
  val allUsers = pi.fold(programUsers)(p => ProgramUserWithRole(p, none) :: programUsers)

object ProposalInfo:
  given Reusability[ProposalInfo] = Reusability.byEq

  val optProposal: Lens[ProposalInfo, Option[Proposal]]      = Focus[ProposalInfo](_.optProposal)
  val minExecutionTime: Lens[ProposalInfo, Option[TimeSpan]] =
    Focus[ProposalInfo](_.minExecutionTime)
  val maxExecutionTime: Lens[ProposalInfo, Option[TimeSpan]] =
    Focus[ProposalInfo](_.maxExecutionTime)
