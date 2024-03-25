// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import explore.model.AppContext
import explore.model.ProgramUserWithRole
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.Divider
import lucuma.ui.primereact.*
import queries.common.InvitationQueriesGQL.*

case class InviteUserPopup(
  pid:     Program.Id,
  addUser: View[AddUserOpen]
) extends ReactFnProps(InviteUserPopup.component)

object InviteUserPopup:
  private object IsOpen extends NewType[Boolean]

  private type Props = InviteUserPopup

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // .useEffectResultOnMountBy: (props, ctx) =>
      //   import ctx.given
      //   CreateInviteMutation[IO].execute(props.pid)
      .render: (props, _) => // , invite) =>
        Dialog(
          visible = props.addUser.get.value,
          onHide = props.addUser.set(AddUserOpen(false)),
          position = DialogPosition.Top,
          closeOnEscape = true,
          closable = true,
          resizable = false,
          clazz = LucumaPrimeStyles.Dialog.Small,
          header = "User Invitation"
        )(
          React.Fragment(
            Divider(),
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              <.label(LucumaPrimeStyles.FormFieldLabel, "ID: "),
              // <.label(LucumaPrimeStyles.FormField, id.show),
              <.label(LucumaPrimeStyles.FormFieldLabel, "Name: "),
              // <.label(LucumaPrimeStyles.FormField, name),
              <.label(LucumaPrimeStyles.FormFieldLabel, "Role: ")
              // <.label(LucumaPrimeStyles.FormField, role)
            )
          )
        )
