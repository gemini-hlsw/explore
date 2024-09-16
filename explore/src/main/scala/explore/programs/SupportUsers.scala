// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import crystal.react.View
import explore.model.ProgramUserWithRole
import lucuma.core.model.Program
import explore.proposal.ProgramUsersTable

case class SupportUsers(
  programId: Program.Id,
  users:     View[List[ProgramUserWithRole]],
  readonly:  Boolean
) extends ReactFnProps(SupportUsers.component)

object SupportUsers:
  private type Props = SupportUsers

  private val component = ScalaFnComponent[Props]: props =>
    ProgramUsersTable(props.programId, props.users, props.readonly)
