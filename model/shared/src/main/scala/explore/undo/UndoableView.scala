// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.effect.kernel.Sync
import crystal.ViewF
import monocle.Lens

/**
 * Weaves `ViewF` and `UndoSetter` logic.
 *
 * When the `ViewF`'s set/mod is invoked, it will be done through the provided
 * `UndoSetter`, such that the change is pushed into the undo stack.
 *
 * The `ViewF` can be zoomed upon via the `apply` methods, which will take either
 * a `Lens` or a `get`/`mod` pair of functions, plus a side effect (`onChange`)
 * to be run whenever the resulting `ViewF`'s value changes, which can happen when
 * the value is directly `set`/`mod`, or when an `undo` or `redo` is executed. This
 * side effect could be, for example, setting the value on a remote DB.
 */
case class UndoableView[F[_]: Sync, G[_], T](undoCtx: UndoContext[F, G, T]) {
  def apply[A](get: T => A, mod: (A => A) => T => T, onChange: A => G[Unit]): ViewF[F, A] = {
    val zoomed = undoCtx.model.zoom(get)(mod)
    ViewF[F, A](
      zoomed.get,
      // To contemplate callbacks, we would need to pass a way to run an F from within G.
      (f, _) => undoCtx.mod[A](get, (a: A) => mod(_ => a), a => onChange(a))(f)
    )
  }

  def apply[A](lens: Lens[T, A], onChange: A => G[Unit]): ViewF[F, A] =
    apply(lens.get, lens.modify, onChange)
}
