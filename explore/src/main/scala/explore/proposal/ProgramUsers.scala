// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import explore.Icons
import explore.components.Tile
import explore.model.AppContext
import explore.model.CoIInvitation
import explore.model.ProgramUserWithRole
import explore.model.ProposalTabTileIds
import explore.model.enums.TileSizeState
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.InvitationStatus
import lucuma.core.model.Program
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.OverlayPanelRef
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import lucuma.core.util.NewType

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
  invitations: View[List[CoIInvitation]]
)(
  val state:   View[ProgramUsersState]
) extends ReactFnProps(ProgramUsers.component)

object ProgramUsers:

  def inviteControl(readOnly: Boolean, ref: OverlayPanelRef)(
    state: View[ProgramUsersState],
    s:     TileSizeState
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
    invitations: View[List[CoIInvitation]],
    ref:         OverlayPanelRef
  )(using AppContext[IO], Logger[IO]) =
    Tile(
      ProposalTabTileIds.UsersId.id,
      ProgramUsersState(CreateInviteProcess.Idle),
      "Investigators"
    )(ProgramUsers(pid, readOnly, users, invitations), inviteControl(readOnly, ref))

  private type Props = ProgramUsers

  private val component =
    ScalaFnComponent[Props]: props =>
      <.div(
        ProgramUsersTable(props.pid, props.users, props.readOnly),
        React
          .Fragment(
            "Pending invitations",
            ProgramUserInvitations(props.invitations, props.readOnly)
          )
          .when(
            props.invitations
              .when(_.filter(_.status === InvitationStatus.Pending).nonEmpty)
          )
      )
