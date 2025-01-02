// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import explore.components.ui.ExploreStyles
import explore.model.Constants
import explore.model.ProgramDetails
import explore.model.ProgramTimes
import explore.model.ProgramUserWithRole
import explore.model.UserInvitation
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Access
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.core.syntax.display.*
import lucuma.react.common.ReactFnProps
import lucuma.ui.primereact.FormInfo

case class ProgramDetailsTile(
  programId:         Program.Id,
  programDetails:    View[ProgramDetails],
  programTimes:      Pot[ProgramTimes],
  semester:          Semester,
  currentUserAccess: Access
) extends ReactFnProps(ProgramDetailsTile.component)

object ProgramDetailsTile:
  private type Props = ProgramDetailsTile

  private val EditSupportAccesses: Set[Access] = Set(Access.Admin, Access.Staff)

  private val component = ScalaFnComponent[Props]: props =>
    val details: ProgramDetails                 = props.programDetails.get
    val thesis: Boolean                         = details.allUsers.exists(_.thesis.exists(_ === true))
    val users: View[List[ProgramUserWithRole]]  = props.programDetails.zoom(ProgramDetails.allUsers)
    val invitations: View[List[UserInvitation]] =
      props.programDetails.zoom(ProgramDetails.invitations)
    val readonly: Boolean                       = !EditSupportAccesses.contains_(props.currentUserAccess)

    <.div(ExploreStyles.ProgramDetailsTile)(
      <.div(ExploreStyles.ProgramDetailsInfoArea, ExploreStyles.ProgramDetailsLeft)(
        FormInfo(details.reference.map(_.label).getOrElse("---"), "Reference"),
        FormInfo(Constants.GppDateFormatter.format(props.semester.start.localDate), "Start"),
        FormInfo(Constants.GppDateFormatter.format(props.semester.end.localDate), "End"),
        // Thesis should be set True if any of the investigators will use the proposal as part of their thesis (3390)
        FormInfo(if (thesis) "Yes" else "No", "Thesis"),
        FormInfo(s"${details.proprietaryMonths} months", "Proprietary")
      ),
      <.div(
        TimeAwardTable(details.allocations),
        TimeAccountingTable(props.programTimes)
      ),
      <.div(ExploreStyles.ProgramDetailsInfoArea)(
        SupportUsers(
          props.programId,
          users,
          invitations,
          "Principal Support",
          SupportUsers.SupportRole.Primary,
          readonly
        ),
        SupportUsers(
          props.programId,
          users,
          invitations,
          "Additional Support",
          SupportUsers.SupportRole.Secondary,
          readonly
        )
        // The two Notifications flags are user-settable and determine whether the archive sends email notifications for new data and whether the ODB sends notifications for expired timing windows (3388, 3389)
        // FormInfo("", "Notifications")
        // The Eavesdropping` UI will allow PIs of accepted programs to select dates when they are available for eavesdropping. This is not needed for XT. (NEED TICKET)
        // FormInfo("", "Eavesdropping")
      )
    )
