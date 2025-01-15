// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.Eq
import monocle.Focus

case class UndoStacks[F[_], M](
  undo:    UndoStack[F, M],
  redo:    UndoStack[F, M],
  working: Boolean
)

object UndoStacks {
  def undo[F[_], M]    = Focus[UndoStacks[F, M]](_.undo)
  def redo[F[_], M]    = Focus[UndoStacks[F, M]](_.redo)
  def working[F[_], M] = Focus[UndoStacks[F, M]](_.working)

  def empty[F[_], M]: UndoStacks[F, M] =
    UndoStacks(List.empty[Restorer[F, M]], List.empty[Restorer[F, M]], false)

  given [F[_], M]: Eq[UndoStacks[F, M]] =
    Eq.by(s => (s.undo, s.redo, s.working))
}
