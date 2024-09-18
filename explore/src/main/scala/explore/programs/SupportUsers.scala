// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.data.NonEmptySet
import crystal.react.View
import explore.model.ProgramUserWithRole
import explore.users.ProgramUsersTable
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.react.common.ReactFnProps
import lucuma.core.enums.ProgramUserRole

case class SupportUsers(
  programId: Program.Id,
  users:     View[List[ProgramUserWithRole]],
  readonly:  Boolean
) extends ReactFnProps(SupportUsers.component)

object SupportUsers:
  private type Props = SupportUsers

  private val component = ScalaFnComponent[Props]: props =>
    ProgramUsersTable(
      props.programId,
      props.users,
      NonEmptySet.of(ProgramUserRole.SupportPrimary, ProgramUserRole.SupportSecondary),
      props.readonly
    )
