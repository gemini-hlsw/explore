// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.CoIInvitation
import explore.model.IsActive
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.InvitationStatus
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.*
import lucuma.react.primereact.Button
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import queries.common.InvitationQueriesGQL.*

case class ProgramUserInvitations(invitations: View[List[CoIInvitation]])
    extends ReactFnProps(ProgramUserInvitations.component)

object ProgramUserInvitations:
  private type Props = ProgramUserInvitations

  private val ColDef = ColumnDef[CoIInvitation]

  private val KeyId: ColumnId    = ColumnId("id")
  private val EmailId: ColumnId  = ColumnId("email")
  private val RevokeId: ColumnId = ColumnId("revoke")

  private val columnNames: Map[ColumnId, String] = Map(
    KeyId    -> "ID",
    EmailId  -> "email",
    RevokeId -> ""
  )

  private def column[V](
    id:       ColumnId,
    accessor: CoIInvitation => V
  ): ColumnDef.Single[CoIInvitation, V] =
    ColDef(id, accessor, columnNames(id))

  private def columns(active: View[IsActive], invitations: View[List[CoIInvitation]])(
    ctx: AppContext[IO]
  ): List[ColumnDef[CoIInvitation, ?]] =
    import ctx.given
    List(
      column(KeyId, _.id),
      column(EmailId, _.email),
      ColDef(
        RevokeId,
        identity,
        "Revoke",
        cell = { cell =>
          val email  = cell.value.email
          val id     = cell.value.id
          val action =
            RevokeInvitationMutation[IO].execute(id) *>
              invitations.mod(_.filterNot(_.id === id)).to[IO]

          val revoke = deleteConfirmation(
            s"This action will revoke the invitation to $email. This action cannot be reversed.",
            "Revoke invitation",
            "Yes, revoke",
            action.void,
            active
          )

          <.div(
            ExploreStyles.ApiKeyDelete,
            Button(
              icon = Icons.Trash,
              severity = Button.Severity.Secondary,
              onClick = revoke,
              tooltip = s"Revoke invitation"
            ).mini.compact
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
      .useMemoBy((_, _, x) => x.reuseByValue)((p, ctx, _) =>
        active => columns(active, p.invitations)(ctx)
      )                           // columns
      .useMemoBy((props, _, _, _) =>
        props.invitations.get.filter(_.status === InvitationStatus.Pending)
      )((_, _, _, _) => identity) // rows
      .useReactTableBy((props, _, _, cols, rows) =>
        TableOptions(cols, rows, getRowId = (row, _, _) => RowId(row.id.toString))
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
