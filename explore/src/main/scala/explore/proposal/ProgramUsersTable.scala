// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

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
import explore.model.display.given
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.Gender
import lucuma.core.enums.Partner
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
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.utils.*
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

  private val ColDef = ColumnDef.WithTableMeta[View[ProgramUserWithRole], TableMeta]

  private val NameColumnId: ColumnId    = ColumnId("name")
  private val PartnerColumnId: ColumnId = ColumnId("Partner")
  private val EmailColumnId: ColumnId   = ColumnId("email")
  private val ESColumnId: ColumnId      = ColumnId("education")
  private val ThesisColumnId: ColumnId  = ColumnId("thesis")
  private val GenderColumnId: ColumnId  = ColumnId("gender")
  private val OrcidIdColumnId: ColumnId = ColumnId("orcid-id")
  private val RoleColumnId: ColumnId    = ColumnId("role")
  private val UnlinkId: ColumnId        = ColumnId("unlink")

  private val columnNames: Map[ColumnId, String] = Map(
    NameColumnId    -> "Name",
    PartnerColumnId -> "Partner",
    EmailColumnId   -> "email",
    ESColumnId      -> "education",
    ThesisColumnId  -> "thesis",
    GenderColumnId  -> "gender",
    OrcidIdColumnId -> "ORCID",
    RoleColumnId    -> "Role",
    UnlinkId        -> ""
  )

  private def column[V](
    id:       ColumnId,
    accessor: View[ProgramUserWithRole] => V
  ): ColumnDef.Single.WithTableMeta[View[ProgramUserWithRole], V, TableMeta] =
    ColDef(id, accessor, columnNames(id))

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
      clazz = ExploreStyles.PartnerSelector,
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
      column(NameColumnId, _.get.name),
      ColDef(
        PartnerColumnId,
        _.zoom(ProgramUserWithRole.partnerLink),
        enableSorting = true,
        enableResizing = true,
        cell = c =>
          c.table.options.meta.map: meta =>

            val cell      = c.row.original
            val userId    = cell.get.user.id
            val usersView = c.value.withOnMod(pl =>
              updateProgramPartner[IO](meta.programId, userId, pl).runAsyncAndForget
            )

            val pl = cell.get.partnerLink.flatMap {
              case PartnerLink.HasUnspecifiedPartner => None
              case p                                 => Some(p)
            }
            partnerSelector(pl, usersView.set, meta.readOnly || meta.isActive.get.value)
      ),
      column(EmailColumnId, _.get.user.profile.foldMap(_.primaryEmail).getOrElse("-")),
      ColDef(
        ESColumnId,
        _.zoom(ProgramUserWithRole.educationalStatus),
        enableSorting = true,
        enableResizing = true,
        cell = c =>

          val cell   = c.row.original
          val userId = cell.get.user.id
          c.table.options.meta.map: meta =>
            val view = c.value
              .withOnMod(es => updateUserES[IO](meta.programId, userId, es).runAsyncAndForget)

            EnumOptionalDropdown[EducationalStatus](
              id = "es".refined,
              value = view.get,
              showClear = true,
              itemTemplate = _.value.shortName,
              valueTemplate = _.value.shortName,
              emptyMessageTemplate = "No Selection",
              disabled = meta.readOnly || meta.isActive.get.value,
              clazz = ExploreStyles.PartnerSelector,
              onChange = view.set
            )
      ),
      ColDef(
        ThesisColumnId,
        _.zoom(ProgramUserWithRole.thesis),
        cell = c =>

          val cell   = c.row.original
          val userId = cell.get.user.id

          c.table.options.meta.map: meta =>
            val view = c.value
              .withOnMod(th => updateUserThesis[IO](meta.programId, userId, th).runAsyncAndForget)
            Checkbox(id = "thesis",
                     checked = view.get.getOrElse(false),
                     disabled = meta.readOnly || meta.isActive.get.value,
                     onChange = r => view.set(r.some)
            )
      ),
      ColDef(
        GenderColumnId,
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
      ),
      column(OrcidIdColumnId, _.get.user.profile.foldMap(_.orcidId.value)),
      column(RoleColumnId, _.get.roleName),
      ColDef(
        UnlinkId,
        identity,
        "",
        enableSorting = false,
        enableResizing = false,
        cell = cell =>
          cell.table.options.meta.map: meta =>

            val userId = cell.value.get.user.id
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
              ).mini.compact.unless(cell.value.get.role.isEmpty) // don't allow removing the PI
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
      .useMemoBy((props, _, _, _) => props.users.reuseByValue): (p, _, _, _) => // rows
        _.toListOfViews
      .useReactTableBy: (props, _, isActive, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => RowId(row.get.user.id.toString),
          meta = TableMeta(props.programId, props.users, props.readOnly, isActive)
        )
      .render: (props, _, _, _, _, table) =>
        PrimeTable(
          table,
          striped = true,
          compact = Compact.Very
        )
