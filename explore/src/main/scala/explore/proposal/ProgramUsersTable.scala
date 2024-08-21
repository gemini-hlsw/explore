// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.common.ProgramQueries.updateProgramUsers
import explore.components.deleteConfirmation
import explore.components.ui.ExploreStyles
import explore.components.ui.PartnerFlags
import explore.model.AppContext
import explore.model.IsActive
import explore.model.ProgramUserWithRole
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Partner
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.SelectItem
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.UnlinkUserInput
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import monocle.function.Each.*
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
  private val PartnerColumnId: ColumnId = ColumnId("Partner")
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

  val partnerLinkOptions: List[PartnerLink] =
    PartnerLink.HasNonPartner :: Enumerated[Partner].all.map { p =>
      PartnerLink.HasPartner(p)
    }

  def partnerItem(pl: PartnerLink): VdomNode = pl match {
    case PartnerLink.HasPartner(p) =>
      <.div(
        ExploreStyles.PartnerFlagItem,
        <.img(
          ^.src := PartnerFlags.smallFlag(p),
          ^.alt := s"${p.shortName} Flag",
          ExploreStyles.PartnerSplitFlag
        ),
        p match {
          case Partner.UH => "U of Hawaii"
          case p          => p.shortName
        }
      )
    case PartnerLink.HasNonPartner => <.div("No Partner")
    case _                         => "Unspecified Partner"
  }

  def partnerSelector(value: Option[PartnerLink], set: Option[PartnerLink] => Callback): VdomNode =
    FormDropdownOptional(
      id = "user-partner-selector".refined,
      placeholder = "Select a partner",
      options = partnerLinkOptions.map { pl =>
        new SelectItem[PartnerLink](value = pl, label = pl.toString)
      },
      clazz = ExploreStyles.PartnerSelector,
      showClear = true,
      itemTemplate = pl => partnerItem(pl.value),
      valueTemplate = pl => partnerItem(pl.value),
      emptyMessageTemplate = "No Selection",
      value = value,
      onChange = set
    )

  def partnerLinkLens(userId: User.Id) =
    each[List[ProgramUserWithRole], ProgramUserWithRole]
      .filter(_.user.id === userId)
      .andThen(ProgramUserWithRole.partnerLink)

  private def columns(
    pid:   Program.Id,
    users: View[List[ProgramUserWithRole]],
    ctx:   AppContext[IO]
  ): List[ColumnDef.WithTableMeta[ProgramUserWithRole, ?, TableMeta]] =
    List(
      column(NameColumnId, _.name),
      ColDef(
        PartnerColumnId,
        identity,
        enableSorting = true,
        enableResizing = true,
        cell = cell =>
          import ctx.given

          val userId    = cell.value.user.id
          val view      = users.zoom(partnerLinkLens(userId))
          val usersView = view.withOnMod(pl =>
            pl.headOption.flatten
              .map(pl => updateProgramUsers[IO](pid, userId, pl).runAsyncAndForget)
              .getOrEmpty
          )

          val pl = cell.value.partnerLink.flatMap {
            case PartnerLink.HasUnspecifiedPartner => None
            case p                                 => Some(p)
          }
          <.div(partnerSelector(pl, usersView.set))
      ),
      column(EmailColumnId, _.user.profile.foldMap(_.primaryEmail).getOrElse("-")),
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
      .useMemoBy((_, _, _) => ()): (p, ctx, _) => // cols
        _ => columns(p.programId, p.users, ctx)
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
