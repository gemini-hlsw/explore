// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.undo

import explore.Icons
import explore.components.ui.ExploreStyles
import explore.syntax.ui.*
import explore.syntax.ui.given
import explore.undo.Undoer
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.typed.primereact.primereactStrings.outlined
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.primereact.Button

case class UndoButtons(
  undoer:   Undoer,
  size:     PlSize = PlSize.Tiny,
  disabled: Boolean = false
) extends ReactFnProps[UndoButtons](UndoButtons.component)

object UndoButtons:
  private type Props = UndoButtons

  private val component =
    ScalaFnComponent[Props](props =>
      <.div(
        ExploreStyles.ButtonsUndo,
        <.span(
          react.common.style.Css("p-buttonset"),
          Button(
            severity = Button.Severity.Secondary,
            outlined = true,
            onClick = props.undoer.undo,
            disabled = props.undoer.isUndoEmpty || props.disabled || props.undoer.working,
            loading = props.undoer.working,
            clazz = props.size.cls,
            icon = Icons.Undo,
            label = "Undo"
          ).compact,
          Button(
            severity = Button.Severity.Secondary,
            outlined = true,
            onClick = props.undoer.redo,
            disabled = props.undoer.isRedoEmpty || props.disabled || props.undoer.working,
            loading = props.undoer.working,
            clazz = props.size.cls,
            icon = Icons.Redo,
            label = "Redo"
          ).compact
        )
      )
    )
