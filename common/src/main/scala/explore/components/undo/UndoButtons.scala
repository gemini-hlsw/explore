// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.undo

import cats.effect.Effect
import crystal.react.implicits._
import explore.Icons
import explore.components.WIP
import explore.undo.Undoer
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps
import react.semanticui.elements.button.Button
import react.semanticui.sizes._

final case class UndoButtons[F[_], A](
  value:      A,
  undoCtx:    Undoer.Context[F, A],
  size:       SemanticSize = Small
)(implicit
  val effect: Effect[F]
) extends ReactProps[UndoButtons[Any, Any]](UndoButtons.component)

object UndoButtons {
  type Props[F[_], A] = UndoButtons[F, A]

  protected def componentBuilder[F[_], A] =
    ScalaComponent
      .builder[Props[F, A]]
      .render_P { p =>
        implicit val effect = p.effect

        WIP(
          <.div(
            Button(onClick = p.undoCtx.undo(p.value).runInCB,
                   size = p.size,
                   disabled = p.undoCtx.undoEmpty
            )(
              Icons.Undo
            ),
            Button(onClick = p.undoCtx.redo(p.value).runInCB,
                   size = p.size,
                   disabled = p.undoCtx.redoEmpty
            )(
              Icons.Redo
            )
          )
        )
      }
      .build

  val component = componentBuilder[Any, Any]
}
