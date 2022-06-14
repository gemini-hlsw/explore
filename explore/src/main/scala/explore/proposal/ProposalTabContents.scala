// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.Eq
import cats.effect.IO
import clue.data.syntax._
import crystal.react.View
import crystal.react.hooks._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
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
import monocle.Focus
import monocle.Lens
import org.typelevel.cats.time.instances.duration._
import queries.common.ProgramQueriesGQL._
import react.common.ReactFnProps

import java.time.Duration

final case class ProposalTabContents(
  programId:  Program.Id,
  user:       Option[User],
  undoStacks: View[UndoStacks[IO, Proposal]]
)(implicit
  val ctx:    AppContextIO
) extends ReactFnProps[ProposalTabContents](ProposalTabContents.component)

object ProposalTabContents {

  protected def renderFn(
    programId:       Program.Id,
    user:            Option[User],
    undoStacks:      View[UndoStacks[IO, Proposal]]
  )(optProposalInfo: View[Option[ProposalInfo]])(implicit ctx: AppContextIO): VdomNode =
    optProposalInfo
      .mapValue((proposalInfo: View[ProposalInfo]) =>
        ProposalEditor(
          programId,
          proposalInfo.zoom(ProposalInfo.proposal),
          undoStacks,
          proposalInfo.get.executionTime,
          NonNegDuration.unsafeFrom(Duration.ofNanos(0)) // Will come from API eventually
        ): VdomNode
      )
      .getOrElse(user match {
        case Some(_: StandardUser) => <.div("Button for creating proposal")
        case _                     => <.div("Log in to create a proposal")
      })

  type Props = ProposalTabContents

  val component = ScalaFnComponent
    .withHooks[Props]
    .useStreamResourceViewOnMountBy { props =>
      implicit val ctx = props.ctx

      ProgramProposalQuery
        .query[IO](props.programId)
        .map(data =>
          data.program
            .flatMap(prog => prog.proposal.map(ProposalInfo(_, prog.plannedTime.execution)))
        )
        .reRunOnResourceSignals(ProgramEditSubscription.subscribe[IO](props.programId.assign))
    }
    .render { (props, optPropInfo) =>
      implicit val ctx = props.ctx
      potRender(renderFn(props.programId, props.user, props.undoStacks) _)(optPropInfo)
    }
}

final case class ProposalInfo(proposal: Proposal, executionTime: NonNegDuration)

object ProposalInfo {
  val proposal: Lens[ProposalInfo, Proposal]            = Focus[ProposalInfo](_.proposal)
  val executionTime: Lens[ProposalInfo, NonNegDuration] = Focus[ProposalInfo](_.executionTime)

  val eqProposalInfo: Eq[ProposalInfo] = Eq.by(pi => (pi.proposal, pi.executionTime))
}
