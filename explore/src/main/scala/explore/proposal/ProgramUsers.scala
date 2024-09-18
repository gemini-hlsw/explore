// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.data.NonEmptySet
import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import explore.components.Tile
import explore.model.ProgramUserWithRole
import explore.model.ProposalTabTileIds
import explore.model.UserInvitation
import explore.users.ProgramUserInvitations
import explore.users.ProgramUsersTable
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.InvitationStatus
import lucuma.core.model.Program
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.OverlayPanelRef
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import lucuma.core.enums.ProgramUserRole

enum CreateInviteProcess(private val tag: String) derives Enumerated:
  case Idle    extends CreateInviteProcess("idle")
  case Running extends CreateInviteProcess("running")
  case Error   extends CreateInviteProcess("error")
  case Done    extends CreateInviteProcess("done")

object ProgramUsersState extends NewType[CreateInviteProcess]
type ProgramUsersState = ProgramUsersState.Type

case class ProgramUsers(
  pid:         Program.Id,
  readOnly:    Boolean,
  users:       View[List[ProgramUserWithRole]],
  invitations: View[List[UserInvitation]],
  state:       View[ProgramUsersState]
) extends ReactFnProps(ProgramUsers.component)

object ProgramUsers:
  private type Props = ProgramUsers

  private def inviteControl(
    readOnly: Boolean,
    ref:      OverlayPanelRef,
    state:    View[ProgramUsersState]
  ) =
    Button(
      severity = Button.Severity.Secondary,
      size = Button.Size.Small,
      disabled = readOnly,
      loading = state.get.value == CreateInviteProcess.Running,
      icon = Icons.UserPlus,
      tooltip = "Create CoI invitation",
      onClickE = ref.toggle
    ).tiny.compact

  def programUsersTile(
    pid:         Program.Id,
    readOnly:    Boolean,
    users:       View[List[ProgramUserWithRole]],
    invitations: View[List[UserInvitation]],
    ref:         OverlayPanelRef
  ) =
    Tile(
      ProposalTabTileIds.UsersId.id,
      "Investigators",
      ProgramUsersState(CreateInviteProcess.Idle)
    )(ProgramUsers(pid, readOnly, users, invitations, _), (s, _) => inviteControl(readOnly, ref, s))

  private val component =
    ScalaFnComponent[Props]: props =>
      <.div(
        ProgramUsersTable(
          props.pid,
          props.users,
          NonEmptySet.of(ProgramUserRole.Pi, ProgramUserRole.Coi, ProgramUserRole.CoiRO),
          props.readOnly
        ),
        React
          .Fragment(
            "Pending invitations",
            ProgramUserInvitations(props.invitations, props.readOnly)
          )
          .when(props.invitations.get.filter(_.status === InvitationStatus.Pending).nonEmpty)
      )
