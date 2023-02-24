// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import eu.timepit.refined.auto.*
import explore.Icons
import explore.Resources
import explore.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Proposal
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import monocle.Focus
import monocle.Lens
import org.typelevel.log4cats.Logger
import queries.common.ProgramQueriesGQL.*
import react.common.ReactFnProps
import react.primereact.Button
import react.primereact.Image

import java.time.Duration

case class ProposalTabContents(
  programId:  Program.Id,
  user:       Option[User],
  undoStacks: View[UndoStacks[IO, Proposal]]
) extends ReactFnProps(ProposalTabContents.component)

object ProposalTabContents:
  private def createProposal(
    programId:       Program.Id,
    optProposalView: View[Option[ProposalInfo]],
    executionTime:   TimeSpan
  )(using TransactionalClient[IO, ObservationDB], Logger[IO]): Callback =
    val proposal = Proposal.Default
    optProposalView.set(ProposalInfo(proposal.some, executionTime).some) >>
      UpdateProgramsMutation
        .execute[IO](
          UpdateProgramsInput(
            WHERE = programId.toWhereProgram.assign,
            SET = ProgramPropertiesInput(proposal = proposal.toInput.assign)
          )
        )
        .void
        .runAsync

  private def renderFn(
    programId:       Program.Id,
    user:            Option[User],
    undoStacks:      View[UndoStacks[IO, Proposal]],
    ctx:             AppContext[IO]
  )(optProposalInfo: View[Option[ProposalInfo]]): VdomNode =
    import ctx.given

    optProposalInfo
      .mapValue { (proposalInfo: View[ProposalInfo]) =>
        val executionTime = proposalInfo.get.executionTime

        proposalInfo
          .zoom(ProposalInfo.optProposal)
          .mapValue((proposalView: View[Proposal]) =>
            ProposalEditor(
              programId,
              proposalView,
              undoStacks,
              executionTime,
              TimeSpan.Zero // Will come from API eventually
            ): VdomNode
          )
          .getOrElse(user match {
            case Some(_: StandardUser) =>
              <.div(
                ExploreStyles.HVCenter,
                Button(
                  label = "Create a Proposal",
                  icon = Icons.FileCirclePlus.withClass(ExploreStyles.OrcidIcon),
                  clazz = ExploreStyles.LoginBoxButton,
                  severity = Button.Severity.Secondary,
                  onClick = createProposal(programId, optProposalInfo, executionTime)
                ).big
              )
            case _                     =>
              <.div(
                ExploreStyles.HVCenter,
                Button(
                  label = "Login with ORCID to create a Proposal",
                  icon = Image(src = Resources.OrcidLogo, clazz = ExploreStyles.OrcidIcon),
                  clazz = ExploreStyles.LoginBoxButton,
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

      ProgramProposalQuery
        .query[IO](props.programId)
        .map(data =>
          data.program.map(prog => ProposalInfo(prog.proposal, prog.plannedTime.execution))
        )
        .reRunOnResourceSignals(ProgramEditSubscription.subscribe[IO](props.programId.assign))
    }
    .render { (props, ctx, optPropInfo) =>
      optPropInfo.render(renderFn(props.programId, props.user, props.undoStacks, ctx) _)
    }

case class ProposalInfo(optProposal: Option[Proposal], executionTime: TimeSpan)

object ProposalInfo:
  val optProposal: Lens[ProposalInfo, Option[Proposal]] = Focus[ProposalInfo](_.optProposal)
  val executionTime: Lens[ProposalInfo, TimeSpan]       = Focus[ProposalInfo](_.executionTime)
