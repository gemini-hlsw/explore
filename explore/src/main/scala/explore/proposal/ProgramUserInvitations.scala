// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.syntax.all.*
import explore.model.ProgramUserWithRole
import explore.model.reusability.given
import japgolly.scalajs.react.*
import lucuma.react.common.ReactFnProps
import lucuma.react.table.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import explore.model.UserInvitation
import explore.model.UserInvitation

case class ProgramUserInvitations(users: List[UserInvitation])
    extends ReactFnProps(ProgramUserInvitations.component)

object ProgramUserInvitations:
  private type Props = ProgramUserInvitations

  private val ColDef = ColumnDef[UserInvitation]

  private val KeyId: ColumnId = ColumnId("id")

  private val columnNames: Map[ColumnId, String] = Map(
    KeyId -> "ID"
  )

  private def column[V](
    id:       ColumnId,
    accessor: UserInvitation => V
  ): ColumnDef.Single[UserInvitation, V] =
    ColDef(id, accessor, columnNames(id))

  private val columns: List[ColumnDef[UserInvitation, ?]] =
    List(
      column(KeyId, _.id)
    )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemo(())(_ => columns)                                // columns
      .useMemoBy((props, _) => props.users)((_, _) => identity) // rows
      .useReactTableBy((props, cols, rows) =>
        TableOptions(cols, rows, getRowId = (row, _, _) => RowId(row.id.toString))
      )
      .render: (props, _, _, table) =>
        PrimeTable(
          table,
          striped = true,
          compact = Compact.Very
        )
