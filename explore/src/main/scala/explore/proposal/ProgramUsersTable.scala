// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.components.deleteConfirmation
import explore.components.ui.ExploreStyles
import explore.components.ui.PartnerFlags
import explore.model.AppContext
import explore.model.IsActive
import explore.model.ProgramUserWithRole
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Button
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.ObservationDB.Types.UnlinkUserInput
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import queries.common.ProposalQueriesGQL.UnlinkUser

case class ProgramUsersTable(
  programId: Program.Id,
  users:     View[List[ProgramUserWithRole]],
  readOnly:  Boolean
) extends ReactFnProps(ProgramUsersTable.component)

object ProgramUsersTable:
  private type Props = ProgramUsersTable

  private case class TableMeta(
    programId: Program.Id,
    users:     View[List[ProgramUserWithRole]],
    readOnly:  Boolean,
    isActive:  View[IsActive]
  )

  private val ColDef = ColumnDef.WithTableMeta[ProgramUserWithRole, TableMeta]

  private val NameColumnId: ColumnId    = ColumnId("name")
  private val PartnerColumnId: ColumnId = ColumnId("partner")
  private val EmailColumnId: ColumnId   = ColumnId("email")
  private val OrcidIdColumnId: ColumnId = ColumnId("orcid-id")
  private val RoleColumnId: ColumnId    = ColumnId("role")
  private val UnlinkId: ColumnId        = ColumnId("unlink")

  private val columnNames: Map[ColumnId, String] = Map(
    NameColumnId    -> "Name",
    PartnerColumnId -> "Partner",
    EmailColumnId   -> "email",
    OrcidIdColumnId -> "ORCID",
    RoleColumnId    -> "Role",
    UnlinkId        -> ""
  )

  private def column[V](
    id:       ColumnId,
    accessor: ProgramUserWithRole => V
  ): ColumnDef.Single.WithTableMeta[ProgramUserWithRole, V, TableMeta] =
    ColDef(id, accessor, columnNames(id))

  private def columns(
    ctx: AppContext[IO]
  ): List[ColumnDef.WithTableMeta[ProgramUserWithRole, ?, TableMeta]] =
    List(
      column(NameColumnId, _.name),
      ColDef(
        PartnerColumnId,
        _.partner,
        "",
        enableSorting = true,
        enableResizing = true,
        cell = _.value.map(partner =>
          <.span(
            <.img(^.src        := PartnerFlags.smallFlag(partner),
                  ^.alt := s"${partner.shortName} Flag",
                  ExploreStyles.PartnerSplitFlag
            )
          ).withTooltip(partner.longName)
        )
      ),
      column(EmailColumnId, _.user.profile.foldMap(_.primaryEmail)),
      column(OrcidIdColumnId, _.user.profile.foldMap(_.orcidId.value)),
      column(RoleColumnId, _.roleName),
      ColDef(
        UnlinkId,
        identity,
        "",
        enableSorting = false,
        enableResizing = false,
        cell = cell =>
          cell.table.options.meta.map: meta =>
            import ctx.given

            val userId = cell.value.user.id
            val action =
              UnlinkUser[IO].execute(UnlinkUserInput(meta.programId, userId)) *>
                meta.users.mod(_.filterNot(_.user.id === userId)).to[IO]

            val unlink = deleteConfirmation(
              s"This action will remove the user from this proposal. This action cannot be reversed.",
              "Remove user",
              "Yes",
              action.void,
              meta.isActive
            )

            <.div(ExploreStyles.ApiKeyDelete)(
              Button(
                icon = Icons.Trash,
                severity = Button.Severity.Secondary,
                disabled = meta.readOnly || meta.isActive.get.value,
                onClick = unlink
              ).mini.compact.unless(cell.value.role.isEmpty) // don't allow removing the PI
            )
        ,
        size = 35.toPx
      )
    )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(IsActive(false))
      .useMemoBy((_, _, _) => ()): (_, ctx, _) => // cols
        _ => columns(ctx)
      .useMemoBy((props, _, _, _) => props.users.get.toList): (_, _, _, _) => // rows
        identity
      .useReactTableBy: (props, _, isActive, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => RowId(row.user.id.toString),
          meta = TableMeta(props.programId, props.users, props.readOnly, isActive)
        )
      .render: (props, _, _, _, _, table) =>
        PrimeTable(
          table,
          striped = true,
          compact = Compact.Very
        )
