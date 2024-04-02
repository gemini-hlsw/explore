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

case class ProgramUsersTable(users: List[ProgramUserWithRole])
    extends ReactFnProps(ProgramUsersTable.component)

object ProgramUsersTable:
  private type Props = ProgramUsersTable

  private val ColDef = ColumnDef[ProgramUserWithRole]

  private val NameColumnId: ColumnId    = ColumnId("name")
  private val EmailColumnId: ColumnId   = ColumnId("email")
  private val OrcidIdColumnId: ColumnId = ColumnId("orcid-id")
  private val RoleColumnId: ColumnId    = ColumnId("role")

  private val columnNames: Map[ColumnId, String] = Map(
    NameColumnId    -> "Name",
    EmailColumnId   -> "email",
    OrcidIdColumnId -> "ORCID",
    RoleColumnId    -> "Role"
  )

  private def column[V](
    id:       ColumnId,
    accessor: ProgramUserWithRole => V
  ): ColumnDef.Single[ProgramUserWithRole, V] =
    ColDef(id, accessor, columnNames(id))

  private val columns: List[ColumnDef[ProgramUserWithRole, ?]] =
    List(
      column(NameColumnId, _.name),
      column(EmailColumnId, _.user.profile.flatMap(_.primaryEmail).orEmpty),
      column(OrcidIdColumnId, _.user.profile.map(_.orcidId.value).orEmpty),
      column(RoleColumnId, _.roleName)
    )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemo(())(_ => columns)                                // columns
      .useMemoBy((props, _) => props.users)((_, _) => identity) // rows
      .useReactTableBy((props, cols, rows) =>
        TableOptions(cols, rows, getRowId = (row, _, _) => RowId(row.user.id.toString))
      )
      .render { (props, _, _, table) =>
        PrimeTable(
          table,
          striped = true,
          compact = Compact.Very
        )
      }
