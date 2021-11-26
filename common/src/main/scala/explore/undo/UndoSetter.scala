// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import crystal.react.View
import crystal.react.implicits._
import japgolly.scalajs.react.util.DefaultEffects.{ Async => DefaultA }
import japgolly.scalajs.react.util.DefaultEffects.{ Sync => DefaultS }
import monocle.Lens

/*
 * Allows modifying values in an undo context, but doesn't give access to undo and redo operations.
 */
trait UndoSetter[M] { self =>
  def model: View[M]

  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => DefaultA[Unit],
    onRestore: (M, A) => DefaultA[Unit]
  )(v:         A): DefaultS[Unit]

  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     A => DefaultA[Unit],
    onRestore: A => DefaultA[Unit]
  )(v:         A): DefaultS[Unit] =
    set(getter, setter, (_: M, a: A) => onSet(a), (_: M, a: A) => onRestore(a))(v)

  def set[A](
    getter: M => A,
    setter: A => M => M,
    onSet:  (M, A) => DefaultA[Unit]
  )(v:      A): DefaultS[Unit] =
    set(getter, setter, onSet, onSet)(v)

  def set[A](
    getter: M => A,
    setter: A => M => M,
    onSet:  A => DefaultA[Unit]
  )(v:      A): DefaultS[Unit] =
    set(getter, setter, (_: M, a: A) => onSet(a))(v)

  def mod[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => DefaultA[Unit],
    onRestore: (M, A) => DefaultA[Unit]
  )(f:         A => A): DefaultS[Unit]

  def mod[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     A => DefaultA[Unit],
    onRestore: A => DefaultA[Unit]
  )(f:         A => A): DefaultS[Unit] =
    mod(getter, setter, (_: M, a: A) => onSet(a), (_: M, a: A) => onRestore(a))(f)

  def mod[A](
    getter: M => A,
    setter: A => M => M,
    onSet:  (M, A) => DefaultA[Unit]
  )(f:      A => A): DefaultS[Unit] =
    mod(getter, setter, onSet, onSet)(f)

  def mod[A](
    getter: M => A,
    setter: A => M => M,
    onSet:  A => DefaultA[Unit]
  )(f:      A => A): DefaultS[Unit] =
    mod(getter, setter, (_: M, a: A) => onSet(a))(f)

  def zoom[N](getN: M => N, modN: (N => N) => (M => M)): UndoSetter[N] =
    new UndoSetter[N] {

      override def model: View[N] = self.model.zoom(getN)(modN)

      override def set[A](
        getter:    N => A,
        setter:    A => (N => N),
        onSet:     (N, A) => DefaultA[Unit],
        onRestore: (N, A) => DefaultA[Unit]
      )(v:         A): DefaultS[Unit] =
        self.set(getter.compose(getN),
                 modN.compose(setter),
                 (m, a) => onSet(getN(m), a),
                 (m, a) => onRestore(getN(m), a)
        )(v)

      override def mod[A](
        getter:    N => A,
        setter:    A => (N => N),
        onSet:     (N, A) => DefaultA[Unit],
        onRestore: (N, A) => DefaultA[Unit]
      )(f:         A => A): DefaultS[Unit] =
        self.mod(getter.compose(getN),
                 modN.compose(setter),
                 (m, a) => onSet(getN(m), a),
                 (m, a) => onRestore(getN(m), a)
        )(f)
    }

  def zoom[N](lens: Lens[M, N]): UndoSetter[N] = zoom(lens.get, lens.modify)

  /**
   * Allows accessing the `UndoSetter` as a `View`.
   *
   * When the `View`'s set/mod is invoked, it will be done through the `UndoSetter`, such that the
   * change is pushed into the undo stack.
   *
   * Using `zoom` into `N` the resulting `View` will still save the whole of `M` in the undo stack.
   * To apply a `zoom` while preserving undo granularity you should `zoom` directly on the
   * `UndoSetter` and create the `undoableView` at the last possible moment.
   */
  def undoableView[N](getN: M => N, modN: (N => N) => M => M): View[N] =
    View[N](
      model.zoom(getN)(modN).get,
      (f, cb) => mod(getN, (n: N) => modN(_ => n), (n: N) => cb(n).to[DefaultA])(f)
    )

  def undoableView[N](lens: Lens[M, N]): View[N] =
    undoableView(lens.get, lens.modify)
}
