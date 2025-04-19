// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import explore.optics.GetAdjust
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA
import japgolly.scalajs.react.util.DefaultEffects.Sync as DefaultS
import monocle.Lens

/*
  Stores parameters needed for UndoContext.set or .mod.
  Allows better encapsulation of undoable actions.
 */
case class Action[M, A](
  getter:    M => A,
  setter:    A => M => M,
  onSet:     (M, A) => DefaultA[Unit],
  onRestore: (M, A) => DefaultA[Unit]
) {
  def set(undoSetter: UndoSetter[M])(v: A): DefaultS[Unit] =
    undoSetter.set(getter, setter, onSet, onRestore)(v)

  def mod(undoSetter: UndoSetter[M])(f: A => A): DefaultS[Unit] =
    undoSetter.mod(getter, setter, onSet, onRestore)(f)
}

object Action {
  def apply[M, A](
    getter: M => A,
    setter: A => M => M,
    onSet:  (M, A) => DefaultA[Unit]
  ): Action[M, A] = Action(getter, setter, onSet, onSet)

  def apply[M, A](
    getter: M => A,
    setter: A => M => M
  ): AppliedGetSet[M, A] =
    new AppliedGetSet[M, A](getter, setter)

  def apply[M, A](access: Lens[M, A]): AppliedGetSet[M, A] =
    new AppliedGetSet[M, A](access.get, access.replace)

  def apply[M, A](access: GetAdjust[M, A]): AppliedGetSet[M, A] =
    new AppliedGetSet[M, A](access.get, access.set)

  class AppliedGetSet[M, A](val getter: M => A, val setter: A => M => M) {
    def apply(
      onSet:     (M, A) => DefaultA[Unit],
      onRestore: (M, A) => DefaultA[Unit]
    ): Action[M, A] =
      Action(getter, setter, onSet, onRestore)

    def apply(
      onSet: (M, A) => DefaultA[Unit]
    ): Action[M, A] =
      Action(getter, setter, onSet, onSet)
  }
}
