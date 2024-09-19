// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.data.NonEmptySet
import crystal.react.*
import explore.model.ProgramUserWithRole
import explore.model.UserInvitation
import explore.users.CreateInviteStatus
import explore.users.ProgramUsersTable
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnProps
import lucuma.ui.syntax.all.given

case class InvestigatorUsers(
  pid:               Program.Id,
  readonly:          Boolean,
  users:             View[List[ProgramUserWithRole]],
  invitations:       View[List[UserInvitation]],
  private val state: View[InvestigatorUsers.State]
) extends ReactFnProps(InvestigatorUsers.component)

object InvestigatorUsers:
  private type Props = InvestigatorUsers

  object State extends NewType[CreateInviteStatus]
  type State = State.Type

  private val component =
    ScalaFnComponent[Props]: props =>
      ProgramUsersTable(
        props.pid,
        props.users,
        props.invitations,
        NonEmptySet.of(ProgramUserRole.Pi, ProgramUserRole.Coi, ProgramUserRole.CoiRO),
        props.readonly
      )
