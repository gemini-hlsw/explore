// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all._
import clue.data.syntax._
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import explore.Icons
import explore.Resources
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.undo._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.NonNegDuration
import lucuma.core.model.Program
import lucuma.core.model.Proposal
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.schemas.ObservationDB.Types._
import monocle.Focus
import monocle.Lens
import queries.common.ProgramQueriesGQL._
import queries.schemas.implicits._
import react.common.ReactFnProps
import react.semanticui.elements.button.Button
import react.semanticui.elements.image.Image
import react.semanticui.sizes.Big

import java.time.Duration

final case class ProposalTabContents(
  programId:  Program.Id,
  user:       Option[User],
  undoStacks: View[UndoStacks[IO, Proposal]]
)(implicit
  val ctx:    AppContextIO
) extends ReactFnProps[ProposalTabContents](ProposalTabContents.component)

object ProposalTabContents {
  private def createProposal(
    programId:       Program.Id,
    optProposalView: View[Option[ProposalInfo]],
    executionTime:   NonNegDuration
  )(implicit ctx:    AppContextIO): Callback = {
    val proposal = Proposal.Default
    optProposalView.set(ProposalInfo(proposal.some, executionTime).some) >>
      EditProgramMutation
        .execute[IO](
          EditProgramInput(
            select = ProgramSelectInput(programId = programId.assign),
            patch = ProgramPropertiesInput(proposal = proposal.toInput.assign)
          )
        )
        .void
        .runAsync
  }

  protected def renderFn(
    programId:       Program.Id,
    user:            Option[User],
    undoStacks:      View[UndoStacks[IO, Proposal]]
  )(optProposalInfo: View[Option[ProposalInfo]])(implicit ctx: AppContextIO): VdomNode =
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
              NonNegDuration.unsafeFrom(Duration.ofNanos(0)) // Will come from API eventually
            ): VdomNode
          )
          .getOrElse(user match {
            case Some(_: StandardUser) =>
              <.div(
                ExploreStyles.HVCenter,
                Button(
                  content = <.div(ExploreStyles.LoginOrcidButton,
                                  Icons.FileCirclePlus.clazz(ExploreStyles.OrcidIcon),
                                  "Create a Proposal"
                  ),
                  size = Big,
                  clazz = ExploreStyles.LoginBoxButton,
                  onClick = createProposal(programId, optProposalInfo, executionTime)
                )
              )
            case _                     =>
              <.div(
                ExploreStyles.HVCenter,
                Button(
                  content = <.div(ExploreStyles.LoginOrcidButton,
                                  Image(clazz = ExploreStyles.OrcidIcon, src = Resources.OrcidLogo),
                                  "Login with ORCID to create a Proposal"
                  ),
                  size = Big,
                  clazz = ExploreStyles.LoginBoxButton,
                  onClick = ctx.sso.switchToORCID.runAsync
                )
              )
          })
      }
      .getOrElse(<.div(s"Program $programId not found!"))

  type Props = ProposalTabContents

  val component = ScalaFnComponent
    .withHooks[Props]
    .useStreamResourceViewOnMountBy { props =>
      implicit val ctx = props.ctx

      ProgramProposalQuery
        .query[IO](props.programId)
        .map(data =>
          data.program.map(prog => ProposalInfo(prog.proposal, prog.plannedTime.execution))
        )
        .reRunOnResourceSignals(ProgramEditSubscription.subscribe[IO](props.programId.assign))
    }
    .render { (props, optPropInfo) =>
      implicit val ctx = props.ctx
      potRender(renderFn(props.programId, props.user, props.undoStacks) _)(optPropInfo)
    }
}

final case class ProposalInfo(optProposal: Option[Proposal], executionTime: NonNegDuration)

object ProposalInfo {
  val optProposal: Lens[ProposalInfo, Option[Proposal]] = Focus[ProposalInfo](_.optProposal)
  val executionTime: Lens[ProposalInfo, NonNegDuration] = Focus[ProposalInfo](_.executionTime)
}
