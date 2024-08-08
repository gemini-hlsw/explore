// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.syntax.eq.*
import crystal.Pot
import explore.components.ui.ExploreStyles
import explore.model.ProgramTimes
import explore.model.ProgramUserWithRole
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.ui.primereact.FormInfo
import explore.model.ProgramDetails
import explore.model.enums.ProgramUserRole
import explore.model.ProgramUser
import explore.model.Proposal
import explore.model.display.given
import explore.model.ProposalType
import lucuma.core.enums.ToOActivation
import lucuma.core.syntax.display.*
import lucuma.core.model.Semester
import explore.model.Constants

case class ProgramDetailsTile(
  programDetails: ProgramDetails,
  programTimes:   Pot[ProgramTimes],
  semester:       Semester
) extends ReactFnProps(ProgramDetailsTile.component)

object ProgramDetailsTile:
  private type Props = ProgramDetailsTile

  val component = ScalaFnComponent[Props]: props =>

    val details: ProgramDetails = props.programDetails
    // val cois: List[ProgramUser]              =
    //   details.users.collect:
    //     case ProgramUserWithRole(user, _, Some(role), _, _) if role === ProgramUserRole.Coi => user
    // val proposal: Option[Proposal] = details.proposal
    // val proposalType: Option[ProposalType] = proposal.flatMap(_.proposalType)
    // val toOActivation: Option[ToOActivation] =
    //   proposalType.flatMap(ProposalType.toOActivation.getOption)

    <.div(ExploreStyles.ProgramDetailsTile)(
      <.div(ExploreStyles.ProgramDetailsInfoArea)(
        FormInfo(details.reference.map(_.label).getOrElse("---"), "Reference"),
        // FormInfo(details.pi.map(_.nameWithEmail).getOrElse("---"), "PI"),
        // FormInfo(cois.map(_.name).mkString(", "), "Co-Investigators"),
        // FormInfo(toOActivation.map(_.label).getOrElse("---"), "ToO Activation"),
        // FormInfo(proposalType.map(_.scienceSubtype.shortName).getOrElse("---"), "Proposal Type"),
        // Start and End are the CfP active interval starting and ending dates.
        FormInfo(Constants.GppDateFormatter.format(props.semester.start.localDate), "Start"),
        FormInfo(Constants.GppDateFormatter.format(props.semester.end.localDate), "End"),
        // Thesis should be set True if any of the investigators will use the proposal as part of their thesis (3390)
        FormInfo("", "Thesis")
        // FormInfo("", "Propietary")
      ),
      <.div(
        TimeAwardTable(details.allocations),
        TimeAccountingTable(props.programTimes)
      ),
      <.div(ExploreStyles.ProgramDetailsInfoArea)(
        // The Contact scientists are the program SUPPORT role which has been requested to be split into two (3278): "Principal Support" and "Additional Support".
        // FormInfo("", "Principal Support"),
        // FormInfo("", "Additional Support"),
        // The two Notifications flags are user-settable and determine whether the archive sends email notifications for new data and whether the ODB sends notifications for expired timing windows (3388, 3389)
        // FormInfo("", "Notifications")
        // The Eavesdropping` UI will allow PIs of accepted programs to select dates when they are available for eavesdropping. This is not needed for XT. (NEED TICKET)
        // FormInfo("", "Eavesdropping")
      )
    )
