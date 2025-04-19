// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import crystal.react.*
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA
import japgolly.scalajs.react.util.DefaultEffects.Sync as DefaultS
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.function.Index.index

/*
 * Allows modifying values in an undo context, but doesn't give access to undo and redo operations.
 */
trait UndoSetter[M] { self =>
  def model: View[M]

  def get: M = model.get

  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => DefaultA[Unit],
    onRestore: (M, A) => DefaultA[Unit]
  )(v: A): DefaultS[Unit]

  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     A => DefaultA[Unit],
    onRestore: A => DefaultA[Unit]
  )(v: A): DefaultS[Unit] =
    set(getter, setter, (_: M, a: A) => onSet(a), (_: M, a: A) => onRestore(a))(v)

  def set[A](
    getter: M => A,
    setter: A => M => M,
    onSet:  (M, A) => DefaultA[Unit]
  )(v: A): DefaultS[Unit] =
    set(getter, setter, onSet, onSet)(v)

  def set[A](
    getter: M => A,
    setter: A => M => M,
    onSet:  A => DefaultA[Unit]
  )(v: A): DefaultS[Unit] =
    set(getter, setter, (_: M, a: A) => onSet(a))(v)

  def mod[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => DefaultA[Unit],
    onRestore: (M, A) => DefaultA[Unit]
  )(f: A => A): DefaultS[Unit]

  def mod[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     A => DefaultA[Unit],
    onRestore: A => DefaultA[Unit]
  )(f: A => A): DefaultS[Unit] =
    mod(getter, setter, (_: M, a: A) => onSet(a), (_: M, a: A) => onRestore(a))(f)

  def mod[A](
    getter: M => A,
    setter: A => M => M,
    onSet:  (M, A) => DefaultA[Unit]
  )(f: A => A): DefaultS[Unit] =
    mod(getter, setter, onSet, onSet)(f)

  def mod[A](
    getter: M => A,
    setter: A => M => M,
    onSet:  A => DefaultA[Unit]
  )(f: A => A): DefaultS[Unit] =
    mod(getter, setter, (_: M, a: A) => onSet(a))(f)

  def zoom[N](getN: M => N, modN: (N => N) => (M => M)): UndoSetter[N] =
    new UndoSetter[N] {
      override def model: View[N] = self.model.zoom(getN)(modN)

      override def get: N = model.get

      override def set[A](
        getter:    N => A,
        setter:    A => (N => N),
        onSet:     (N, A) => DefaultA[Unit],
        onRestore: (N, A) => DefaultA[Unit]
      )(v: A): DefaultS[Unit] =
        self.set(
          getter.compose(getN),
          modN.compose(setter),
          (m, a) => onSet(getN(m), a),
          (m, a) => onRestore(getN(m), a)
        )(v)

      override def mod[A](
        getter:    N => A,
        setter:    A => (N => N),
        onSet:     (N, A) => DefaultA[Unit],
        onRestore: (N, A) => DefaultA[Unit]
      )(f: A => A): DefaultS[Unit] =
        self.mod(
          getter.compose(getN),
          modN.compose(setter),
          (m, a) => onSet(getN(m), a),
          (m, a) => onRestore(getN(m), a)
        )(f)
    }

  def zoom[N](lens: Lens[M, N]): UndoSetter[N] = zoom(lens.get, lens.modify)

  def zoom[N](optional: Optional[M, N]): Option[UndoSetter[N]] =
    // _.get is safe here since it's only being called when the value is defined.
    // The zoom getter function is stored to use in callbacks, so we have to pass _.get
    // instead of capturing the value here. Otherwise, callbacks see a stale value.
    optional.getOption(get).map(_ => zoom(m => optional.getOption(m).get, optional.modify))

  def zoom[N](prism: Prism[M, N]): Option[UndoSetter[N]] =
    // _.get is safe here since it's only being called when the value is defined.
    // The zoom getter function is stored to use in callbacks, so we have to pass _.get
    // instead of capturing the value here. Otherwise, callbacks see a stale value.
    prism.getOption(get).map(_ => zoom(m => prism.getOption(m).get, prism.modify))

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
      getN(get),
      (f, cb) =>
        mod(
          getN,
          (n: N) => modN(_ => n),
          (n: N) => cb(getN(get), n).to[DefaultA]
        )(f)
    )

  def undoableView[N](lens: Lens[M, N]): View[N] =
    undoableView(lens.get, lens.modify)

  /**
   * Converts an UndoSetter[List[A]] into a List[UndoSetter[A]].
   */
  def toListOfUndoSetters[A](using ev: M =:= List[A]): List[UndoSetter[A]] =
    get.indices.toList
      .map: i =>
        val atIndex                   = index[List[A], Int, A](i)
        val getter: M => A            = m => atIndex.getOption.andThen(_.get)(ev.flip(m))
        val mod: (A => A) => (M => M) = f => l => ev.flip(atIndex.modify(f)(l))
        zoom(getter, mod)
}
