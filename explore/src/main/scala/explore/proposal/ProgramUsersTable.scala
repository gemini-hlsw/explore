// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.components.deleteConfirmation
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.IsActive
import explore.model.ProgramUserWithRole
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.ObservationDB.Types.UnlinkUserInput
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import queries.common.ProposalQueriesGQL.UnlinkUser

case class ProgramUsersTable(pid: Program.Id, users: View[NonEmptyList[ProgramUserWithRole]])
    extends ReactFnProps(ProgramUsersTable.component)

object ProgramUsersTable:
  private type Props = ProgramUsersTable

  private val ColDef = ColumnDef[ProgramUserWithRole]

  private val NameColumnId: ColumnId    = ColumnId("name")
  private val EmailColumnId: ColumnId   = ColumnId("email")
  private val OrcidIdColumnId: ColumnId = ColumnId("orcid-id")
  private val RoleColumnId: ColumnId    = ColumnId("role")
  private val UnlinkId: ColumnId        = ColumnId("unlink")

  private val columnNames: Map[ColumnId, String] = Map(
    NameColumnId    -> "Name",
    EmailColumnId   -> "email",
    OrcidIdColumnId -> "ORCID",
    RoleColumnId    -> "Role",
    UnlinkId        -> ""
  )

  private def column[V](
    id:       ColumnId,
    accessor: ProgramUserWithRole => V
  ): ColumnDef.Single.NoMeta[ProgramUserWithRole, V] =
    ColDef(id, accessor, columnNames(id))

  private def columns(
    props:  Props,
    active: View[IsActive]
  )(ctx: AppContext[IO]): List[ColumnDef.NoMeta[ProgramUserWithRole, ?]] =
    List(
      column(NameColumnId, _.name),
      column(EmailColumnId, _.user.profile.flatMap(_.primaryEmail).orEmpty),
      column(OrcidIdColumnId, _.user.profile.map(_.orcidId.value).orEmpty),
      column(RoleColumnId, _.roleName),
      ColDef(
        UnlinkId,
        identity,
        "",
        enableSorting = false,
        enableResizing = false,
        cell = { cell =>
          import ctx.given
          val uid    = cell.value.user.id
          val action =
            UnlinkUser[IO].execute(UnlinkUserInput(props.pid, uid)) *>
              props.users.mod(t => t.copy(tail = t.tail.filterNot(_.user.id === uid))).to[IO]

          val unlink = deleteConfirmation(
            s"This action will remove the user from this proposal. This action cannot be reversed.",
            "Remove user",
            "Yes",
            action.void,
            active
          )

          <.div(
            ExploreStyles.ApiKeyDelete,
            Button(
              icon = Icons.Trash,
              severity = Button.Severity.Secondary,
              disabled = active.get.value,
              onClick = unlink
            ).mini.compact.unless(cell.value.role.isEmpty) // don't allow removing the PI
          )
        },
        size = 35.toPx
      )
    )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(IsActive(false))
      .useMemoBy((_, _, x) => x.reuseByValue)((p, ctx, _) => a => columns(p, a)(ctx))  // columns
      .useMemoBy((props, _, _, _) => props.users.get.toList)((_, _, _, _) => identity) // rows
      .useReactTableBy((props, _, _, cols, rows) =>
        TableOptions(cols, rows, getRowId = (row, _, _) => RowId(row.user.id.toString))
      )
      .render: (props, _, _, _, _, table) =>
        PrimeTable(
          table,
          striped = true,
          compact = Compact.Very
        )
