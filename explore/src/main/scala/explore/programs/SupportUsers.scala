// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import crystal.react.View
import explore.components.ui.ExploreStyles
import explore.model.ProgramUser
import explore.users.ProgramUsersTable
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.react.common.ReactFnProps

case class SupportUsers(
  programId:   Program.Id,
  users:       View[List[ProgramUser]],
  title:       String,
  supportRole: SupportUsers.SupportRole
) extends ReactFnProps(SupportUsers.component)

object SupportUsers:
  private type Props = SupportUsers

  enum SupportRole(protected[SupportUsers] val mode: ProgramUsersTable.Mode):
    case Primary   extends SupportRole(ProgramUsersTable.Mode.SupportPrimary)
    case Secondary extends SupportRole(ProgramUsersTable.Mode.SupportSecondary)

  private val component = ScalaFnComponent[Props]: props =>
    <.div(ExploreStyles.ProgramDetailsUsers)(
      <.label(
        props.title
      ),
      ProgramUsersTable(
        props.users,
        props.supportRole.mode
      )
    )
