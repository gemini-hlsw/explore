// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.undo

import explore.Icons
import explore.components.ui.ExploreStyles
import explore.syntax.ui.*
import explore.syntax.ui.given
import explore.undo.UndoContext
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.typed.primereact.primereactStrings.outlined
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.primereact.Button

case class UndoButtons[A](
  undoCtx:  UndoContext[A],
  size:     PlSize = PlSize.Tiny,
  disabled: Boolean = false
) extends ReactFnProps[UndoButtons[Any]](UndoButtons.component)

object UndoButtons {
  type Props[A] = UndoButtons[A]

  protected def componentBuilder[A] =
    ScalaFnComponent { (p: Props[A]) =>
      <.div(
        ExploreStyles.ButtonsUndo,
        <.span(
          react.common.style.Css("p-buttonset"),
          Button(
            severity = Button.Severity.Secondary,
            outlined = true,
            onClick = p.undoCtx.undo,
            disabled = p.undoCtx.isUndoEmpty || p.disabled || p.undoCtx.working,
            loading = p.undoCtx.working,
            clazz = p.size.cls,
            icon = Icons.Undo,
            label = "Undo"
          ).compact,
          Button(
            severity = Button.Severity.Secondary,
            outlined = true,
            onClick = p.undoCtx.redo,
            disabled = p.undoCtx.isRedoEmpty || p.disabled || p.undoCtx.working,
            loading = p.undoCtx.working,
            clazz = p.size.cls,
            icon = Icons.Redo,
            label = "Redo"
          ).compact
        )
      )
    }

  val component = componentBuilder[Any]
}
