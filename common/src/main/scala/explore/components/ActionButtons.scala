// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.option.*
import explore.Icons
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

case class ActionButtons(
  copyProps:   ActionButtons.ButtonProps,
  pasteProps:  ActionButtons.ButtonProps,
  deleteProps: ActionButtons.ButtonProps
) extends ReactFnProps(ActionButtons.component)

object ActionButtons:
  private type Props = ActionButtons

  case class ButtonProps(
    callback:     Callback,
    disabled:     Boolean = false,
    tooltipExtra: Option[String] = none
  ):
    val tooltipExtraStr: String = tooltipExtra.map(" " + _).orEmpty

  private val component =
    ScalaFnComponent[Props]: props =>
      <.span(Css("p-button-group"))(
        Button(
          icon = Icons.Clone.withFixedWidth(),
          onClick = props.copyProps.callback,
          tooltip = "Copy" + props.copyProps.tooltipExtraStr,
          tooltipOptions = ToolbarTooltipOptions.Default,
          disabled = props.copyProps.disabled,
          severity = Button.Severity.Secondary
        ).compact.mini,
        Button(
          icon = Icons.Clipboard.withFixedWidth(),
          onClick = props.pasteProps.callback,
          tooltip = "Paste" + props.pasteProps.tooltipExtraStr,
          tooltipOptions = ToolbarTooltipOptions.Default,
          disabled = props.pasteProps.disabled,
          severity = Button.Severity.Secondary,
          clazz = PlSize.Mini.cls
        ).compact.mini,
        Button(
          icon = Icons.Trash.withFixedWidth(),
          onClick = props.deleteProps.callback,
          tooltip = "Delete" + props.deleteProps.tooltipExtraStr,
          tooltipOptions = ToolbarTooltipOptions.Default,
          disabled = props.deleteProps.disabled,
          severity = Button.Severity.Secondary
        ).compact.mini
      )
