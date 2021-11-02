// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.undo

// import crystal.react.implicits._
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps
import react.semanticui.elements.button._
import react.semanticui.shorthand._
import react.semanticui.sizes._

final case class UndoButtons[A](
  undoCtx:  UndoCtx[A],
  size:     SemanticSize = Tiny,
  disabled: Boolean = false
) extends ReactProps[UndoButtons[Any]](UndoButtons.component)

object UndoButtons {
  type Props[A] = UndoButtons[A]

  protected def componentBuilder[A] =
    ScalaComponent
      .builder[Props[A]]
      .render_P { p =>
        <.span(
          ExploreStyles.ButtonsUndo,
          ButtonGroup(labeled = true, icon = true, compact = true, size = p.size)(
            Button(
              onClick = p.undoCtx.undo,
              size = p.size,
              disabled = p.undoCtx.isUndoEmpty || p.disabled || p.undoCtx.working,
              loading = p.undoCtx.working,
              clazz = ExploreStyles.VeryCompact,
              icon = Icons.Undo,
              content = "Undo",
              labelPosition = LabelPosition.Left
            ),
            Button(
              onClick = p.undoCtx.redo,
              size = p.size,
              disabled = p.undoCtx.isRedoEmpty || p.disabled || p.undoCtx.working,
              loading = p.undoCtx.working,
              clazz = ExploreStyles.VeryCompact,
              icon = Icons.Redo,
              content = "Redo",
              labelPosition = LabelPosition.Left
            )
          )
        )
      }
      .build

  val component = componentBuilder[Any]
}
