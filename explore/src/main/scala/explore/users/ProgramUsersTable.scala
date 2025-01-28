// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.users

import cats.data.NonEmptySet
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.ProgramQueries.*
import explore.components.deleteConfirmation
import explore.components.ui.ExploreStyles
import explore.components.ui.PartnerFlags
import explore.model.AppContext
import explore.model.ExploreModelValidators
import explore.model.IsActive
import explore.model.ProgramUser
import explore.model.ProgramUser.Status.*
import explore.model.User
import explore.model.UserInvitation
import explore.model.UserInvitation.DeliveryStatus.*
import explore.model.display.given
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.Gender
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.PartnerLink
import lucuma.core.syntax.all.*
import lucuma.core.util.Enumerated
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Checkbox
import lucuma.react.primereact.OverlayPanelRef
import lucuma.react.primereact.SelectItem
import lucuma.react.primereact.TooltipOptions
import lucuma.react.primereact.hooks.all.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.react.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.utils.*
import monocle.function.Each.*
import queries.common.InvitationQueriesGQL.RevokeInvitationMutation
import queries.common.ProposalQueriesGQL.DeleteProgramUser

case class ProgramUsersTable(
  userId:             Option[User.Id],
  users:              View[List[ProgramUser]],
  filterRoles:        NonEmptySet[ProgramUserRole],
  proposalIsReadonly: Boolean,
  userIsReadonlyCoi:  Boolean,
  hiddenColumns:      Set[ProgramUsersTable.Column] = Set.empty
) extends ReactFnProps(ProgramUsersTable.component):
  val proposalOrUserIsReadonly: Boolean = proposalIsReadonly || userIsReadonlyCoi

object ProgramUsersTable:
  private type Props = ProgramUsersTable

  private case class TableMeta(
    userId:             Option[User.Id],
    users:              View[List[ProgramUser]],
    proposalIsReadonly: Boolean,
    userIsReadonlyCoi:  Boolean,
    isActive:           View[IsActive],
    overlayPanelRef:    OverlayPanelRef,
    createInviteStatus: View[CreateInviteStatus],
    currentProgUser:    View[Option[View[ProgramUser]]]
  ):
    val proposalOrUserIsReadonly: Boolean            = proposalIsReadonly || userIsReadonlyCoi
    // for the user specific fields - readonly COIs can edit their own.
    def currentUserCanEdit(pu: ProgramUser): Boolean =
      !proposalIsReadonly &&
        (!userIsReadonlyCoi || (userId, pu.user).mapN((uid, p) => uid === p.id).getOrElse(false))

  private val ColDef = ColumnDef.WithTableMeta[View[ProgramUser], TableMeta]

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
    case Status            extends Column("status", "Status")
    case Actions           extends Column("actions", "")

  private def column[V](
    column:   Column,
    accessor: View[ProgramUser] => V
  ): ColumnDef.Single.WithTableMeta[View[ProgramUser], V, TableMeta] =
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
    each[List[ProgramUser], ProgramUser]
      .filter(_.user.exists(_.id === userId))

  private def deleteUserButton(
    programUserId: ProgramUser.Id,
    users:         View[List[ProgramUser]],
    isActive:      View[IsActive],
    readOnly:      Boolean
  )(using ctx: AppContext[IO]): VdomNode =
    import ctx.given

    val action =
      DeleteProgramUser[IO].execute(programUserId.toDeleteInput) *>
        users.mod(_.filterNot(_.id === programUserId)).to[IO]

    val delete = deleteConfirmation(
      s"This action will remove the user from this proposal. This action cannot be reversed.",
      "Remove user",
      "Yes",
      action.void,
      isActive
    )
    Button(
      icon = Icons.Trash,
      severity = Button.Severity.Secondary,
      disabled = readOnly || isActive.get.value,
      onClick = delete,
      tooltip = "Remove user from proposal",
      tooltipOptions = TooltipOptions.Left
    ).mini.compact

  private def inviteUserButton(
    programUser: View[ProgramUser],
    tableMeta:   TableMeta
  ): VdomNode =
    Button(
      severity = Button.Severity.Secondary,
      disabled =
        tableMeta.proposalOrUserIsReadonly || tableMeta.isActive.get.value || tableMeta.createInviteStatus.get == CreateInviteStatus.Running,
      icon = Icons.PaperPlaneTop,
      tooltip = s"Send invitation",
      tooltipOptions = TooltipOptions.Left,
      onClickE = e =>
        tableMeta.currentProgUser.set(programUser.some) >>
          tableMeta.overlayPanelRef.toggle(e)
    ).mini.compact

  private def revokeInvitationButton(
    programUser: View[ProgramUser],
    invitation:  UserInvitation,
    isActive:    View[IsActive],
    readOnly:    Boolean
  )(using ctx: AppContext[IO]): VdomNode =
    import ctx.given

    val action: IO[Unit] =
      RevokeInvitationMutation[IO].execute(invitation.id) *>
        programUser.zoom(ProgramUser.invitations).mod(_.filterNot(_.id === invitation.id)).to[IO]

    val revoke: Callback =
      deleteConfirmation(
        s"This action will revoke the invitation to ${invitation.email}. This action cannot be reversed.",
        "Revoke invitation",
        "Yes, revoke",
        action.void,
        isActive
      )

    Button(
      icon = Icons.Ban,
      severity = Button.Severity.Secondary,
      disabled = readOnly || isActive.get.value,
      onClick = revoke,
      tooltip = s"Revoke invitation",
      tooltipOptions = TooltipOptions.Left
    ).mini.compact

  private def statusIcon(status: ProgramUser.Status): TagMod =
    status match
      case Confirmed               => TagMod.empty
      case NotInvited              => TagMod.empty
      case Invited(deliveryStatus) =>
        deliveryStatus match
          case Success(message)    => <.span(Icons.SuccessCheckmark).withTooltip(message)
          case InProgress(message) => <.span(Icons.Info).withTooltip(message)
          case Failed(message)     => <.span(Icons.ErrorIcon).withTooltip(message)

  private val CoIRoles: Set[ProgramUserRole]    = Set(ProgramUserRole.Coi, ProgramUserRole.CoiRO)
  private val NonCoIRoles: Set[ProgramUserRole] = Enumerated[ProgramUserRole].all.toSet -- CoIRoles

  private def columns(using
    ctx: AppContext[IO]
  ): List[ColumnDef.WithTableMeta[View[ProgramUser], ?, TableMeta]] =
    import ctx.given

    List(
      ColDef(
        Column.Name.id,
        identity,
        Column.Name.header,
        cell = c =>
          c.table.options.meta.map: meta =>
            val pu: ProgramUser               = c.value.get
            val programUserId: ProgramUser.Id = pu.id
            // We'll allow editing the name if there is no REAL user
            // AND the fallback display name is the same as the credit name.
            // In explore, we'll always set the credit name, but a user could have
            // updated the fallback profile via the API, and we won't mess with that.
            if (
              !meta.proposalIsReadonly && pu.user.isEmpty && pu.fallbackProfile.creditName === pu.fallbackProfile.displayName
            )
              val view = c.value
                .zoom(ProgramUser.fallbackCreditName)
                .withOnMod(ones =>
                  updateUserFallbackName[IO](programUserId, ones.map(_.value)).runAsync
                )
              FormInputTextView(
                id = NonEmptyString.unsafeFrom(s"${programUserId}-name"),
                value = view,
                disabled = meta.isActive.get.value,
                validFormat = InputValidSplitEpi.nonEmptyString.optional
              ): VdomNode
            else pu.name: VdomNode
      ).sortableBy(_.get.name),
      ColDef(
        Column.Email.id,
        identity,
        Column.Email.header,
        cell = c =>
          c.table.options.meta.map: meta =>
            val pu: ProgramUser               = c.value.get
            val programUserId: ProgramUser.Id = pu.id
            val canEdit                       = meta.currentUserCanEdit(pu)
            // We'll allow editing the email if there is no REAL user
            // or the real email address is empty
            if (canEdit && pu.user.flatMap(_.profile).flatMap(_.email).isEmpty)
              val view = c.value
                .zoom(ProgramUser.fallbackEmail)
                .withOnMod(oe =>
                  updateUserFallbackEmail[IO](programUserId, oe.map(_.value.value)).runAsync
                )
              FormInputTextView(
                id = NonEmptyString.unsafeFrom(s"${programUserId}-email"),
                value = view,
                disabled = meta.isActive.get.value,
                validFormat = ExploreModelValidators.MailValidator.optional
              ): VdomNode
            else pu.email.getOrElse("-"): VdomNode
      ).sortableBy(_.get.email),
      ColDef(
        Column.Partner.id,
        _.zoom(ProgramUser.partnerLink),
        enableSorting = true,
        enableResizing = true,
        cell = c =>
          c.table.options.meta.map: meta =>
            val cell: View[ProgramUser]              = c.row.original
            val programUserId: ProgramUser.Id        = cell.get.id
            val usersView: View[Option[PartnerLink]] =
              c.value.withOnMod: pl =>
                updateProgramPartner[IO](programUserId, pl).runAsync
            val pl: Option[PartnerLink]              =
              cell.get.partnerLink.flatMap:
                case PartnerLink.HasUnspecifiedPartner => None
                case p                                 => Some(p)
            val canEdit                              = meta.currentUserCanEdit(cell.get)

            partnerSelector(pl, usersView.set, !canEdit || meta.isActive.get.value)
      ).sortableBy(_.get.toString),
      ColDef(
        Column.EducationalStatus.id,
        _.zoom(ProgramUser.educationalStatus),
        enableSorting = true,
        enableResizing = true,
        cell = c =>
          val cell                          = c.row.original
          val programUserId: ProgramUser.Id = cell.get.id
          c.table.options.meta.map: meta =>
            val view: View[Option[EducationalStatus]] =
              c.value.withOnMod: es =>
                updateUserES[IO](programUserId, es).runAsync
            val canEdit                               = meta.currentUserCanEdit(cell.get)

            EnumDropdownOptionalView(
              id = NonEmptyString.unsafeFrom(s"$programUserId-es"),
              value = view,
              showClear = true,
              itemTemplate = _.value.shortName,
              valueTemplate = _.value.shortName,
              emptyMessageTemplate = "No Selection",
              disabled = !canEdit || meta.isActive.get.value,
              clazz = ExploreStyles.PartnerSelector
            )
      ).sortableBy(_.get.toString),
      ColDef(
        Column.Thesis.id,
        _.zoom(ProgramUser.thesis),
        cell = c =>
          val cell                          = c.row.original
          val programUserId: ProgramUser.Id = cell.get.id

          c.table.options.meta.map: meta =>
            val view    = c.value
              .withOnMod(th => updateUserThesis[IO](programUserId, th).runAsync)
            val canEdit = meta.currentUserCanEdit(cell.get)

            Checkbox(
              id = s"$programUserId-thesis",
              checked = view.get.getOrElse(false),
              disabled = !canEdit || meta.isActive.get.value,
              onChange = r => view.set(r.some)
            )
        ,
        size = 80.toPx
      ).sortableBy(_.get),
      ColDef(
        Column.Gender.id,
        _.zoom(ProgramUser.gender),
        cell = c =>
          val cell                          = c.row.original
          val programUserId: ProgramUser.Id = cell.get.id

          c.table.options.meta.map: meta =>
            val view    = c.value
              .withOnMod(th => updateUserGender[IO](programUserId, th).runAsync)
            val canEdit = meta.currentUserCanEdit(cell.get)

            EnumOptionalDropdown[Gender](
              id = NonEmptyString.unsafeFrom(s"$programUserId-gender"),
              value = view.get,
              showClear = true,
              itemTemplate = _.value.shortName,
              valueTemplate = _.value.shortName,
              emptyMessageTemplate = "No Selection",
              disabled = !canEdit || meta.isActive.get.value,
              clazz = ExploreStyles.PartnerSelector,
              onChange = view.set
            )
      ).sortableBy(_.get.toString),
      column(Column.OrcidId, _.get.user.flatMap(_.orcidId).foldMap(_.value)).sortable,
      // TODO: Make editable between COI and Readonly COI, if user is not this one
      // column(Column.Role, _.get.role.shortName).sortable,
      ColDef(
        Column.Role.id,
        _.zoom(ProgramUser.role),
        Column.Role.header,
        cell = c =>
          val currentRole = c.value.get
          c.table.options.meta.map: meta =>
            if (meta.proposalOrUserIsReadonly || !CoIRoles.contains(currentRole))
              currentRole.shortName: VdomNode
            else
              val programUserId = c.row.original.get.id
              val view          =
                c.value.withOnMod(role => changeProgramUserRole[IO](programUserId, role).runAsync)
              EnumDropdownView(
                id = NonEmptyString.unsafeFrom(s"$programUserId-role"),
                value = view,
                exclude = NonCoIRoles,
                clazz = ExploreStyles.PartnerSelector
              ): VdomNode
      ).sortableBy(_.get.shortName),
      ColDef(
        Column.Status.id,
        _.get.status,
        Column.Status.header,
        cell = c =>
          val status = c.value
          val icon   = statusIcon(status)
          <.span(icon, " ", status.message)
      ).sortableBy(_.message),
      ColDef(
        Column.Actions.id,
        identity,
        "",
        enableResizing = false,
        cell = cell =>
          cell.table.options.meta.map: meta =>
            val programUserView = cell.value
            val programUserId   = programUserView.get.id
            val role            = programUserView.get.role
            val status          = programUserView.get.status

            <.span(
              deleteUserButton(
                programUserId,
                meta.users,
                meta.isActive,
                meta.proposalOrUserIsReadonly
              )
                .unless(role === ProgramUserRole.Pi), // don't allow removing the PI
              inviteUserButton(programUserView, meta)
                .when(status === ProgramUser.Status.NotInvited),
              programUserView.get.activeInvitation
                .map(invitation =>
                  revokeInvitationButton(
                    programUserView,
                    invitation,
                    meta.isActive,
                    meta.proposalOrUserIsReadonly
                  )
                    .when(status.isInvited)
                )
                .orEmpty
            )
        ,
        size = 85.toPx
      )
    )

  private val component =
    ScalaFnComponent[Props](props =>
      for {
        ctx                <- useContext(AppContext.ctx)
        isActive           <- useStateView(IsActive(false))
        cols               <- useMemo(()): _ =>
                                columns(using ctx)
        rows               <- useMemo(props.users.reuseByValue)(
                                _.toListOfViews.filter(row => props.filterRoles.contains_(row.get.role))
                              )
        overlayPanelRef    <- useOverlayPanelRef
        currentProgUser    <- useStateView(none[View[ProgramUser]])
        _                  <- useEffectWithDeps(rows): rows =>
                                // we need to set it when possible so the invite user popup
                                // gets in the DOM. It will be re-set on button click
                                currentProgUser.set(rows.headOption)
        createInviteStatus <- useStateView(CreateInviteStatus.Idle)
        table              <- useReactTable(
                                TableOptions(
                                  cols,
                                  rows,
                                  getRowId = (row, _, _) => RowId(row.get.id.toString),
                                  enableSorting = true,
                                  meta = TableMeta(
                                    props.userId,
                                    props.users,
                                    props.proposalIsReadonly,
                                    props.userIsReadonlyCoi,
                                    isActive,
                                    overlayPanelRef,
                                    createInviteStatus,
                                    currentProgUser
                                  ),
                                  state = PartialTableState(
                                    columnVisibility = ColumnVisibility(
                                      (props.hiddenColumns.map(_.id -> Visibility.Hidden) +
                                        (Column.Actions.id -> Visibility.fromVisible(
                                          !props.proposalOrUserIsReadonly
                                        ))).toMap
                                    )
                                  )
                                )
                              )
      } yield React.Fragment(
        PrimeTable(
          table,
          striped = true,
          compact = Compact.Very,
          emptyMessage = "No users defined"
        ),
        currentProgUser.get.map(progUser =>
          InviteUserPopup(progUser, createInviteStatus, overlayPanelRef) // : VdomNode
        )
      )
    )
