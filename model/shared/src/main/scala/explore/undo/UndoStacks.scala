// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.Eq
import monocle.macros.Lenses

@Lenses
case class UndoStacks[F[_], M](
  undo:    UndoStack[F, M],
  redo:    UndoStack[F, M],
  working: Boolean
)
object UndoStacks {
  def empty[F[_], M]: UndoStacks[F, M] =
    UndoStacks(List.empty[Restorer[F, M]], List.empty[Restorer[F, M]], false)

  implicit def eqUndoStacks[F[_], M]: Eq[UndoStacks[F, M]] =
    Eq.by(s => (s.undo, s.redo, s.working))
}
