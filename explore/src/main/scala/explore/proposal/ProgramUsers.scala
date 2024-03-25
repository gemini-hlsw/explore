// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ProgramUserWithRole
import explore.model.ProposalTabTileIds
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.OverlayPanel
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import queries.common.InvitationQueriesGQL.*
import explore.model.UserInvitation

object AddUserOpen extends NewType[Boolean]
type AddUserOpen = AddUserOpen.Type

enum CreateInviteProcess:
  case Idle, Running, Done

case class ProgramUsers(
  pid:         Program.Id,
  users:       List[ProgramUserWithRole],
  invitations: List[UserInvitation],
  addUser:     View[CreateInviteProcess]
) extends ReactFnProps(ProgramUsers.component)

object ProgramUsers:
  // def createInvitation(
  //   createInvite: View[CreateInviteProcess],
  //   pid:          Program.Id
  // )(using ctx: AppContext[IO]): IO[Unit] =
  //   import ctx.given
  //   (createInvite.set(CreateInviteProcess.Running).to[IO] *> CreateInviteMutation[IO].execute(
  //     pid
  //   )).void
  //     .guarantee(
  //       createInvite.set(CreateInviteProcess.Done).to[IO]
  //     )

  def programUsersTile(
    pid:          Program.Id,
    users:        List[ProgramUserWithRole],
    invitations:  List[UserInvitation],
    createInvite: View[CreateInviteProcess]
  )(using AppContext[IO], Logger[IO]) = {

    val control =
      <.div(
        ExploreStyles.JustifiedEndTileControl,
        Button(
          severity = Button.Severity.Secondary,
          size = Button.Size.Small,
          loading = createInvite.get == CreateInviteProcess.Running,
          icon = Icons.UserPlus,
          tooltip = "Create CoI invitation"
          // onClick = createInvitation(createInvite, pid).runAsync
        ).tiny.compact
      )

    Tile(ProposalTabTileIds.UsersId.id,
         "Investigators",
         canMinimize = true,
         control = _ => control.some
    )(_ => ProgramUsers(pid, users, invitations, createInvite))
  }

  private type Props = ProgramUsers

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(CreateInviteProcess.Idle)
      .render: (props, create) =>
        <.div(
          OverlayPanel(true)(
            Button(icon = Icons.UserPlus, tooltip = "Create CoI invitation").tiny.compact
          ),
          // if (props.addUser.get.value) InviteUserPopup(props.pid, props.users, props.addUser)
          // else EmptyVdom,
          ProgramUsersTable(props.users),
          "Pending invitations".when(props.invitations.nonEmpty),
          ProgramUserInvitations(props.invitations).when(props.invitations.nonEmpty)
        )
