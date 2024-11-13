// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.syntax.all.*
import crystal.react.hooks.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.Divider
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given

case class ConfigurationRequestEditorPopup(
  trigger:  Button,
  onSubmit: String => Callback
) extends ReactFnProps(ConfigurationRequestEditorPopup.component)

object ConfigurationRequestEditorPopup:
  private type Props = ConfigurationRequestEditorPopup

  private object PopupState extends NewType[Boolean]:
    inline def Open: PopupState   = PopupState(true)
    inline def Closed: PopupState = PopupState(false)

  private type PopupState = PopupState.Type

  val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useStateView(PopupState.Closed)
    .useStateView("") // message
    .render: (props, ctx, popupState, message) =>
      val close = popupState.set(PopupState.Closed)

      val notice =
        """Please briefly describe and justify the requested changes to the approved
          |coordinates + instrument configurations + constraints. These changes will
          |be reviewed by the Head of Science Operations at the site of the observations.
        """.stripMargin.linesIterator.mkString(" ")

      val footer = React.Fragment(
        Button(
          label = "Clear Text",
          icon = Icons.Eraser,
          disabled = message.get.isEmpty,
          onClick = message.set("")
        ).small,
        Button(
          label = "Cancel",
          icon = Icons.Close,
          severity = Button.Severity.Danger,
          onClick = close
        ).small,
        Button(
          label = "Submit",
          icon = Icons.PaperPlaneTop,
          disabled = message.get.isBlank,
          onClick = close >> props.onSubmit(message.get)
        ).small
      )

      React.Fragment(
        props.trigger.copy(onClick = props.trigger.onClick >> popupState.set(PopupState.Open)),
        Dialog(
          visible = popupState.get.value,
          onHide = close,
          closable = true,
          closeOnEscape = true,
          dismissableMask = true,
          resizable = true,
          clazz = LucumaPrimeStyles.Dialog.Small |+| ExploreStyles.ConfigurationRequestEditorPopup,
          header = "Request Editor",
          footer = footer
        )(
          <.div(
            notice,
            Divider(),
            FormInputTextAreaView(
              id = "message_text_area".refined,
              value = message
            )
          )
        )
      )