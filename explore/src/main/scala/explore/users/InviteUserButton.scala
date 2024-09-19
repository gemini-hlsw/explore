// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.users

import crystal.react.View
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import explore.Icons
import lucuma.react.primereact.hooks.all.*
import lucuma.core.enums.ProgramUserRole
import explore.model.display.given
import lucuma.core.syntax.display.*
import lucuma.core.model.Program
import explore.model.UserInvitation
import crystal.react.hooks.*

case class InviteUserButton(
  programId:   Program.Id,
  role:        ProgramUserRole,
  invitations: View[List[UserInvitation]]
) extends ReactFnProps(InviteUserButton.component)

object InviteUserButton:
  private type Props = InviteUserButton

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useOverlayPanelRef
      .useStateView(CreateInviteStatus.Idle)
      .render: (props, overlayRef, createInviteStatus) =>
        React.Fragment(
          InviteUserPopup(
            props.programId,
            ProgramUserRole.Coi,
            props.invitations,
            createInviteStatus,
            overlayRef
          ),
          Button(
            severity = Button.Severity.Secondary,
            size = Button.Size.Small,
            loading = createInviteStatus.get == CreateInviteStatus.Running,
            icon = Icons.UserPlus,
            tooltip = s"Create ${props.role.shortName} invitation",
            onClickE = overlayRef.toggle
          ).tiny.compact
        )
