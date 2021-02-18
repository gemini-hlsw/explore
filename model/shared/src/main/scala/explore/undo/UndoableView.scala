// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.effect.Async
import cats.effect.ContextShift
import cats.syntax.all._
import crystal.ViewF
import monocle.Lens

/**
 * Weaves `ViewF` and `Undoer` logic.
 *
 * When the `ViewF`'s set/mod is modified, it will be done through the provided
 * `Undoer.Setter`, such that the change is pushed into the `Undoer` stack.
 *
 * The `ViewF` can be zoomed upon via the `apply` methods, which will take either
 * a `Lens` or a `get`/`mod` pair of functions, plus a side effect (`onChange`)
 * to be run whenever the resulting `ViewF`'s value changes, which can happen when
 * the value is directly `set`/`mod`, or when an `undo` or `redo` is executed. This
 * side effect could be, for example, setting the value on a remote DB.
 */
case class UndoableView[F[_]: Async: ContextShift, T](
  view:   ViewF[F, T],
  setter: Undoer.Setter[F, T]
) {
  def apply[A](get: T => A, mod: (A => A) => T => T, onChange: A => F[Unit]): ViewF[F, A] = {
    val zoomed = view.zoom(get)(mod)
    ViewF(
      zoomed.get,
      setter.mod(
        view.get,
        get,
        a => (zoomed.set(a) >> onChange(a))
      )
    )
  }

  def apply[A](lens: Lens[T, A], onChange: A => F[Unit]): ViewF[F, A] =
    apply(lens.get, lens.modify, onChange)
}
