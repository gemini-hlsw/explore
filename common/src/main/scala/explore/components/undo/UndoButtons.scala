// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.undo

import explore.Icons
import explore.components.ui.ExploreStyles
import explore.syntax.ui.*
import explore.syntax.ui.given
import explore.undo.UndoContext
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.semanticui.elements.button._
import react.semanticui.sizes._

final case class UndoButtons[A](
  undoCtx:  UndoContext[A],
  size:     SemanticSize = Tiny,
  disabled: Boolean = false
) extends ReactFnProps[UndoButtons[Any]](UndoButtons.component)

object UndoButtons {
  type Props[A] = UndoButtons[A]

  protected def componentBuilder[A] =
    ScalaFnComponent { (p: Props[A]) =>
      <.div(
        ExploreStyles.ButtonsUndo,
        ButtonGroup(labeled = true, icon = true, compact = true, size = p.size)(
          Button(
            onClick = p.undoCtx.undo,
            size = p.size,
            disabled = p.undoCtx.isUndoEmpty || p.disabled || p.undoCtx.working,
            loading = p.undoCtx.working,
            clazz = ExploreStyles.VeryCompact,
            labelPosition = LabelPosition.Left,
            icon = true
          )(Icons.Undo, <.span(ExploreStyles.ButtonsUndoLabel, "Undo")),
          Button(
            onClick = p.undoCtx.redo,
            size = p.size,
            disabled = p.undoCtx.isRedoEmpty || p.disabled || p.undoCtx.working,
            loading = p.undoCtx.working,
            clazz = ExploreStyles.VeryCompact,
            icon = true,
            labelPosition = LabelPosition.Left
          )(Icons.Redo, <.span(ExploreStyles.ButtonsUndoLabel, "Redo"))
        )
      )
    }

  val component = componentBuilder[Any]
}
