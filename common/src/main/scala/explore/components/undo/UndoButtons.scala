// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.undo

import explore.Icons
import explore.components.ToolbarTooltipOptions
import explore.components.ui.ExploreStyles
import explore.undo.Undoer
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css
import lucuma.react.primereact.Button
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

case class UndoButtons(
  undoer:   Undoer,
  size:     PlSize = PlSize.Tiny,
  disabled: Boolean = false
) extends ReactFnProps[UndoButtons](UndoButtons.component)

object UndoButtons:
  private type Props = UndoButtons

  private val component =
    ScalaFnComponent[Props](props =>
      <.span(ExploreStyles.ButtonsUndo, Css("p-button-group"))(
        Button(
          severity = Button.Severity.Secondary,
          outlined = true,
          onClick = props.undoer.undo,
          disabled = props.undoer.isUndoEmpty || props.disabled || props.undoer.working,
          loading = props.undoer.working,
          clazz = props.size.cls,
          icon = Icons.Undo,
          tooltip = "Undo",
          tooltipOptions = ToolbarTooltipOptions.Default
        ).compact,
        Button(
          severity = Button.Severity.Secondary,
          outlined = true,
          onClick = props.undoer.redo,
          disabled = props.undoer.isRedoEmpty || props.disabled || props.undoer.working,
          loading = props.undoer.working,
          clazz = props.size.cls,
          icon = Icons.Redo,
          tooltip = "Redo",
          tooltipOptions = ToolbarTooltipOptions.Default
        ).compact
      )
    )
