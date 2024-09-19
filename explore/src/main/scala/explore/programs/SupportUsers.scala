// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.data.NonEmptySet
import crystal.react.View
import explore.model.ProgramUserWithRole
import explore.model.UserInvitation
import explore.users.ProgramUsersTable
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.react.common.ReactFnProps
import explore.users.InviteUserButton

case class SupportUsers(
  programId:   Program.Id,
  users:       View[List[ProgramUserWithRole]],
  invitations: View[List[UserInvitation]],
  title:       String,
  supportRole: SupportUsers.SupportRole,
  readonly:    Boolean
) extends ReactFnProps(SupportUsers.component)

object SupportUsers:
  private type Props = SupportUsers

  enum SupportRole(protected[SupportUsers] val role: ProgramUserRole):
    case Primary   extends SupportRole(ProgramUserRole.SupportPrimary)
    case Secondary extends SupportRole(ProgramUserRole.SupportSecondary)

  private val component = ScalaFnComponent[Props]: props =>
    <.div(
      <.label(
        props.title,
        InviteUserButton(props.programId, props.supportRole.role, props.invitations)
      ),
      ProgramUsersTable(
        props.programId,
        props.users,
        props.invitations,
        NonEmptySet.one(props.supportRole.role),
        props.readonly,
        hiddenColumns = Set(
          ProgramUsersTable.Column.Partner,
          ProgramUsersTable.Column.EducationalStatus,
          ProgramUsersTable.Column.Thesis,
          ProgramUsersTable.Column.Gender,
          ProgramUsersTable.Column.Role,
          ProgramUsersTable.Column.OrcidId
        )
      )
    )
