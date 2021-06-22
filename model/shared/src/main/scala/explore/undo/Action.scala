// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import explore.optics.GetAdjust
import monocle.Lens

/*
  Stores parameters needed for UndoContext.set or .mod.
  Allows better encapsulation of undoable actions.
 */
case class Action[G[_], M, A](
  getter:    M => A,
  setter:    A => M => M,
  onSet:     (M, A) => G[Unit],
  onRestore: (M, A) => G[Unit]
) {
  def set[F[_]](undoCtx: UndoContext[F, G, M])(v: A): F[Unit] =
    undoCtx.set(getter, setter, onSet, onRestore)(v)

  def mod[F[_]](undoCtx: UndoContext[F, G, M])(f: A => A): F[Unit] =
    undoCtx.mod(getter, setter, onSet, onRestore)(f)
}

object Action {
  def apply[G[_]]: Applied[G] = new Applied[G]

  def apply[G[_], M, A](
    getter: M => A,
    setter: A => M => M,
    onSet:  (M, A) => G[Unit]
  ): Action[G, M, A] = Action(getter, setter, onSet, onSet)

  class Applied[G[_]] {
    def apply[M, A](
      getter: M => A,
      setter: A => M => M
    ): AppliedGetSet[G, M, A] =
      new AppliedGetSet[G, M, A](getter, setter)

    def apply[M, A](lens: Lens[M, A]): AppliedGetSet[G, M, A] =
      new AppliedGetSet[G, M, A](lens.get, lens.set)

    def apply[M, A](getAdjust: GetAdjust[M, A]): AppliedGetSet[G, M, A] =
      new AppliedGetSet[G, M, A](getAdjust.get, getAdjust.set)
  }

  class AppliedGetSet[G[_], M, A](val getter: M => A, val setter: A => M => M) {
    def apply(
      onSet:     (M, A) => G[Unit],
      onRestore: (M, A) => G[Unit]
    ): Action[G, M, A] =
      Action(getter, setter, onSet, onRestore)

    def apply(
      onSet: (M, A) => G[Unit]
    ): Action[G, M, A] =
      Action(getter, setter, onSet, onSet)
  }
}
