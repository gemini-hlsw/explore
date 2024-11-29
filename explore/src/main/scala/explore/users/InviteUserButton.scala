// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.users

import crystal.react.View
import crystal.react.hooks.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.UserInvitation
import explore.model.display.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.core.syntax.display.*
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.hooks.all.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

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
        <.div(
          ExploreStyles.InviteUserButton,
          InviteUserPopup(
            props.programId,
            props.role,
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
