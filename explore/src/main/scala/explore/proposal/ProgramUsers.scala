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
import explore.components.Tile.RenderInTitle
import explore.components.ui.ExploreStyles
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

enum CreateInviteProcess(private val tag: String) derives Enumerated:
  case Idle    extends CreateInviteProcess("idle")
  case Running extends CreateInviteProcess("running")
  case Error   extends CreateInviteProcess("error")
  case Done    extends CreateInviteProcess("done")

case class ProgramUsers(
  pid:         Program.Id,
  users:       View[NonEmptyList[ProgramUserWithRole]],
  invitations: View[List[CoIInvitation]],
  inTitle:     RenderInTitle,
  ref:         OverlayPanelRef
) extends ReactFnProps(ProgramUsers.component)

object ProgramUsers:

  def programUsersTile(
    pid:          Program.Id,
    users:        View[NonEmptyList[ProgramUserWithRole]],
    invitations:  View[List[CoIInvitation]],
    createInvite: View[CreateInviteProcess],
    ref:          OverlayPanelRef
  )(using AppContext[IO], Logger[IO]) = {

    val control =
      <.div(
        ExploreStyles.JustifiedEndTileControl,
        InviteUserPopup(pid, ref, invitations),
        Button(
          severity = Button.Severity.Secondary,
          size = Button.Size.Small,
          loading = createInvite.get == CreateInviteProcess.Running,
          icon = Icons.UserPlus,
          tooltip = "Create CoI invitation",
          onClickE = ref.toggle
        ).tiny.compact
      )

    Tile(ProposalTabTileIds.UsersId.id,
         "Investigators",
         canMinimize = true,
         control = s => control.some.filter(_ => s === TileSizeState.Minimized)
    )(r => ProgramUsers(pid, users, invitations, r, ref))
  }

  private type Props = ProgramUsers

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(CreateInviteProcess.Idle)
      .render: (props, create) =>
        <.div(
          InviteUserPopup(props.pid, props.ref, props.invitations),
          props.inTitle(
            Button(
              severity = Button.Severity.Secondary,
              size = Button.Size.Small,
              icon = Icons.UserPlus,
              tooltip = "Create CoI invitation",
              onClickE = props.ref.toggle
            ).tiny.compact
          ),
          ProgramUsersTable(props.pid, props.users),
          React
            .Fragment(
              "Pending invitations",
              ProgramUserInvitations(props.invitations)
            )
            .when(props.invitations.when(_.filter(_.status === InvitationStatus.Pending).nonEmpty))
        )
