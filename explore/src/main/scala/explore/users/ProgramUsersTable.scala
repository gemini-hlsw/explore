// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.users

import cats.data.NonEmptySet
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.common.ProgramQueries.*
import explore.components.deleteConfirmation
import explore.components.ui.ExploreStyles
import explore.components.ui.PartnerFlags
import explore.model.AppContext
import explore.model.IsActive
import explore.model.ProgramUserWithRole
import explore.model.UserInvitation
import explore.model.display.given
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.Gender
import lucuma.core.enums.InvitationStatus
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.syntax.all.*
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Checkbox
import lucuma.react.primereact.SelectItem
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.UnlinkUserInput
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.react.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.utils.*
import monocle.function.Each.*
import queries.common.ProposalQueriesGQL.UnlinkUser

case class ProgramUsersTable(
  programId:     Program.Id,
  users:         View[List[ProgramUserWithRole]],
  invitations:   View[List[UserInvitation]],
  filterRoles:   NonEmptySet[ProgramUserRole],
  readonly:      Boolean,
  hiddenColumns: Set[ProgramUsersTable.Column] = Set.empty
) extends ReactFnProps(ProgramUsersTable.component)

object ProgramUsersTable:
  private type Props = ProgramUsersTable

  private case class TableMeta(
    programId: Program.Id,
    users:     View[List[ProgramUserWithRole]],
    readOnly:  Boolean,
    isActive:  View[IsActive]
  )

  private val ColDef = ColumnDef.WithTableMeta[View[ProgramUserWithRole], TableMeta]

  enum Column(
    protected[ProgramUsersTable] val tag:    String,
    protected[ProgramUsersTable] val header: String
  ):
    val id: ColumnId = ColumnId(tag)

    case Name              extends Column("name", "Name")
    case Partner           extends Column("partner", "Partner")
    case Email             extends Column("email", "Email")
    case EducationalStatus extends Column("education", "Education")
    case Thesis            extends Column("thesis", "Thesis")
    case Gender            extends Column("gender", "Gender")
    case OrcidId           extends Column("orcid-id", "ORCID")
    case Role              extends Column("role", "Role")
    case Unlink            extends Column("unlink", "")

  private def column[V](
    column:   Column,
    accessor: View[ProgramUserWithRole] => V
  ): ColumnDef.Single.WithTableMeta[View[ProgramUserWithRole], V, TableMeta] =
    ColDef(column.id, accessor, column.header)

  val partnerLinkOptions: List[PartnerLink] =
    PartnerLink.HasNonPartner :: Enumerated[Partner].all.map { p =>
      PartnerLink.HasPartner(p)
    }

  private def partnerItem(pl: PartnerLink): VdomNode = pl match {
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

  private def partnerSelector(
    value:    Option[PartnerLink],
    set:      Option[PartnerLink] => Callback,
    readOnly: Boolean
  ): VdomNode =
    FormDropdownOptional(
      id = "user-partner-selector".refined,
      placeholder = "Select a partner",
      options = partnerLinkOptions.map { pl =>
        new SelectItem[PartnerLink](value = pl, label = pl.toString)
      },
      clazz = ExploreStyles.PartnerSelector |+| ExploreStyles.WarningInput.when_(value.isEmpty),
      showClear = true,
      disabled = readOnly,
      itemTemplate = pl => partnerItem(pl.value),
      valueTemplate = pl => partnerItem(pl.value),
      emptyMessageTemplate = "No Selection",
      value = value,
      onChange = set
    )

  def userWithRole(userId: User.Id) =
    each[List[ProgramUserWithRole], ProgramUserWithRole]
      .filter(_.user.id === userId)

  private def columns(
    ctx: AppContext[IO]
  ): List[ColumnDef.WithTableMeta[View[ProgramUserWithRole], ?, TableMeta]] =
    import ctx.given

    List(
      column(Column.Name, _.get.name).sortable,
      ColDef(
        Column.Partner.id,
        _.zoom(ProgramUserWithRole.partnerLink),
        enableSorting = true,
        enableResizing = true,
        cell = c =>
          c.table.options.meta.map: meta =>
            val cell: View[ProgramUserWithRole]      = c.row.original
            val userId: User.Id                      = cell.get.user.id
            val usersView: View[Option[PartnerLink]] =
              c.value.withOnMod: pl =>
                updateProgramPartner[IO](meta.programId, userId, pl).runAsyncAndForget
            val pl: Option[PartnerLink]              =
              cell.get.partnerLink.flatMap:
                case PartnerLink.HasUnspecifiedPartner => None
                case p                                 => Some(p)

            partnerSelector(pl, usersView.set, meta.readOnly || meta.isActive.get.value)
      ).sortableBy(_.get.toString),
      column(Column.Email, _.get.user.profile.foldMap(_.email).getOrElse("-")).sortable,
      ColDef(
        Column.EducationalStatus.id,
        _.zoom(ProgramUserWithRole.educationalStatus),
        enableSorting = true,
        enableResizing = true,
        cell = c =>
          val cell   = c.row.original
          val userId = cell.get.user.id
          c.table.options.meta.map: meta =>
            val view: View[Option[EducationalStatus]] =
              c.value.withOnMod: es =>
                updateUserES[IO](meta.programId, userId, es).runAsyncAndForget

            EnumDropdownOptionalView(
              id = "es".refined,
              value = view,
              showClear = true,
              itemTemplate = _.value.shortName,
              valueTemplate = _.value.shortName,
              emptyMessageTemplate = "No Selection",
              disabled = meta.readOnly || meta.isActive.get.value,
              clazz = ExploreStyles.PartnerSelector
            )
      ).sortableBy(_.get.toString),
      ColDef(
        Column.Thesis.id,
        _.zoom(ProgramUserWithRole.thesis),
        cell = c =>
          val cell   = c.row.original
          val userId = cell.get.user.id

          c.table.options.meta.map: meta =>
            val view = c.value
              .withOnMod(th => updateUserThesis[IO](meta.programId, userId, th).runAsyncAndForget)
            Checkbox(
              id = "thesis",
              checked = view.get.getOrElse(false),
              disabled = meta.readOnly || meta.isActive.get.value,
              onChange = r => view.set(r.some)
            )
      ).sortableBy(_.get),
      ColDef(
        Column.Gender.id,
        _.zoom(ProgramUserWithRole.gender),
        cell = c =>
          val cell   = c.row.original
          val userId = cell.get.user.id

          c.table.options.meta.map: meta =>
            val view = c.value
              .withOnMod(th => updateUserGender[IO](meta.programId, userId, th).runAsyncAndForget)
            EnumOptionalDropdown[Gender](
              id = "gender".refined,
              value = view.get,
              showClear = true,
              itemTemplate = _.value.shortName,
              valueTemplate = _.value.shortName,
              emptyMessageTemplate = "No Selection",
              disabled = meta.readOnly || meta.isActive.get.value,
              clazz = ExploreStyles.PartnerSelector,
              onChange = view.set
            )
      ).sortableBy(_.get.toString),
      column(Column.OrcidId, _.get.user.orcidId.foldMap(_.value)).sortable,
      column(Column.Role, _.get.role.shortName).sortable,
      ColDef(
        Column.Unlink.id,
        _.get,
        "",
        enableResizing = false,
        cell = cell =>
          cell.table.options.meta.map: meta =>
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
              ).mini.compact
                .unless(cell.value.role === ProgramUserRole.Pi) // don't allow removing the PI
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
      .useMemoBy((props, _, _, _) => props.users.reuseByValue): (props, _, _, _) => // rows
        _.toListOfViews.filter(row => props.filterRoles.contains_(row.get.role))
      .useReactTableBy: (props, _, isActive, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => RowId(row.get.user.id.toString),
          enableSorting = true,
          meta = TableMeta(props.programId, props.users, props.readonly, isActive),
          state = PartialTableState(
            columnVisibility = ColumnVisibility(
              (props.hiddenColumns.map(_.id -> Visibility.Hidden) +
                (Column.Unlink.id -> Visibility.fromVisible(!props.readonly))).toMap
            )
          )
        )
      .render: (props, _, _, _, _, table) =>
        val arePendingInvitations: Boolean = props.invitations.get
          .filter: i =>
            i.status === InvitationStatus.Pending && props.filterRoles.contains_(i.role)
          .nonEmpty

        React.Fragment(
          PrimeTable(
            table,
            striped = true,
            compact = Compact.Very,
            emptyMessage = "No users defined"
          ),
          Option
            .when[VdomNode](arePendingInvitations)(
              React.Fragment(
                <.label("Pending invitations"),
                ProgramUserInvitations(props.invitations, props.filterRoles, props.readonly)
              )
            )
            .orEmpty
        )
