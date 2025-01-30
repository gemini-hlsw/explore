// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import crystal.react.syntax.effect.*
import explore.common.ProgramQueries
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.ProgramDetails
import explore.model.ProgramTimes
import explore.model.ProgramUser
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.core.syntax.display.*
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.primereact.CheckboxView
import lucuma.ui.primereact.FormInfo
import lucuma.ui.primereact.given

case class ProgramDetailsTile(
  programId:         Program.Id,
  programDetails:    View[ProgramDetails],
  programTimes:      Pot[ProgramTimes],
  semester:          Semester,
  userIsReadonlyCoi: Boolean
) extends ReactFnProps(ProgramDetailsTile.component)

object ProgramDetailsTile:
  private type Props = ProgramDetailsTile

  private val component = ScalaFnComponent[Props]: props =>
    useContext(AppContext.ctx).map: ctx =>
      import ctx.given

      val details: ProgramDetails        = props.programDetails.get
      val thesis: Boolean                = details.allUsers.exists(_.thesis.exists(_ === true))
      val users: View[List[ProgramUser]] = props.programDetails.zoom(ProgramDetails.allUsers)
      val newDataNotificationView        =
        props.programDetails
          .zoom(ProgramDetails.shouldNotify)
          .withOnMod(b => ProgramQueries.updateGoaShouldNotify[IO](props.programId, b).runAsync)

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
            "Principal Support",
            SupportUsers.SupportRole.Primary
          ),
          SupportUsers(
            props.programId,
            users,
            "Additional Support",
            SupportUsers.SupportRole.Secondary
          ),

          // The two Notifications flags are user-settable and determine whether the archive sends email notifications for new data and whether the ODB sends notifications for expired timing windows (3388, 3389)
          <.div(
            ExploreStyles.ProgramDetailsRight,
            FormInfo(
              CheckboxView(
                id = "shouldNotify".refined,
                value = newDataNotificationView,
                label = "New Science Data",
                disabled = props.userIsReadonlyCoi
              ),
              "Notifications"
            )
            // FormInfo(
            //   CheckboxView(
            //     id = "expiredTimingWindows".refined,
            //     value = ???,
            //     label = "Expired Timing Windows"
            //   ),
            //   ""
            // )
          )

          // The Eavesdropping` UI will allow PIs of accepted programs to select dates when they are available for eavesdropping. This is not needed for XT. (NEED TICKET)
          // <.div(
          //   ExploreStyles.ProgramDetailsRight,
          //   FormInfo(
          //     CheckboxView(
          //       id = "eavesdropping".refined,
          //       value = ???,
          //       label = ??? // instead of a label there will be a date picker or something?
          //     ),
          //     "Eavesdropping"
          //   )
          // )
        )
      )
