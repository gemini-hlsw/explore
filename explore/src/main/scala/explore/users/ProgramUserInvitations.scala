// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.users

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.IsActive
import explore.model.UserInvitation
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.InvitationStatus
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.*
import lucuma.react.primereact.Button
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import queries.common.InvitationQueriesGQL.*

case class ProgramUserInvitations(invitations: View[List[UserInvitation]], readOnly: Boolean)
    extends ReactFnProps(ProgramUserInvitations.component)

object ProgramUserInvitations:
  private type Props = ProgramUserInvitations

  private case class TableMeta(
    isActive:    View[IsActive],
    invitations: View[List[UserInvitation]],
    readOnly:    Boolean
  )

  private val ColDef = ColumnDef.WithTableMeta[UserInvitation, TableMeta]

  private val KeyId: ColumnId         = ColumnId("id")
  private val EmailId: ColumnId       = ColumnId("email")
  private val EmailStatusId: ColumnId = ColumnId("emailStatus")
  private val RevokeId: ColumnId      = ColumnId("revoke")

  private val columnNames: Map[ColumnId, String] = Map(
    KeyId         -> "ID",
    EmailId       -> "email",
    EmailStatusId -> "",
    RevokeId      -> ""
  )

  private def column[V](
    id:       ColumnId,
    accessor: UserInvitation => V
  ): ColumnDef.Single.WithTableMeta[UserInvitation, V, TableMeta] =
    ColDef(id, accessor, columnNames(id))

  private def columns(
    ctx: AppContext[IO]
  ): List[ColumnDef.WithTableMeta[UserInvitation, ?, TableMeta]] =
    import ctx.given

    List(
      column(KeyId, _.id),
      column(EmailId, _.email),
      ColDef(
        EmailStatusId,
        _.emailStatus,
        "Email Status",
        cell = _.value
          .map(es => <.span(es.tag.toUpperCase).withTooltip(es.description))
          .getOrElse(<.span())
      ),
      ColDef(
        RevokeId,
        identity,
        "Revoke",
        cell = cell =>
          cell.table.options.meta.map: meta =>
            val email = cell.value.email
            val id    = cell.value.id

            val action: IO[Unit] =
              RevokeInvitationMutation[IO].execute(id) *>
                meta.invitations.mod(_.filterNot(_.id === id)).to[IO]

            val revoke: Callback =
              Callback.log(s"Revoke invitation to $email") >>
                deleteConfirmation(
                  s"This action will revoke the invitation to $email. This action cannot be reversed.",
                  "Revoke invitation",
                  "Yes, revoke",
                  action.void,
                  meta.isActive
                )

            <.div(ExploreStyles.ApiKeyDelete)(
              Button(
                icon = Icons.Trash,
                severity = Button.Severity.Secondary,
                disabled = meta.readOnly || meta.isActive.get.value,
                onClick = revoke,
                tooltip = s"Revoke invitation"
              ).mini.compact
            )
        ,
        size = 35.toPx
      )
    )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(IsActive(false)) // isActive
      .useMemoBy((_, _, _) => ()): (_, ctx, _) => // cols
        _ => columns(ctx)
      .useMemoBy((props, _, _, _) => // rows
        props.invitations.get.filter(_.status === InvitationStatus.Pending)
      )((_, _, _, _) => identity)
      .useReactTableBy: (props, _, isActive, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => RowId(row.id.toString),
          meta = TableMeta(
            isActive = isActive,
            invitations = props.invitations,
            readOnly = props.readOnly
          )
        )
      .render: (props, _, _, _, _, table) =>
        React.Fragment(
          PrimeTable(
            table,
            striped = true,
            compact = Compact.Very,
            tableMod = ExploreStyles.ExploreTable
          )
        )
