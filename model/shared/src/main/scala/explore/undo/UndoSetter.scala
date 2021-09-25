// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.Monad
import cats.~>
import crystal.ViewF
import monocle.Lens

/*
 * Allows modifying values in an undo context, but doesn't give access to undo and redo operations.
 */
trait UndoSetter[F[_], G[_], M] { self =>
  def model: ViewF[F, M]
  val syncToAsync: F ~> G
  implicit val F: Monad[F]

  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => G[Unit],
    onRestore: (M, A) => G[Unit]
  )(v:         A): F[Unit]

  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     A => G[Unit],
    onRestore: A => G[Unit]
  )(v:         A): F[Unit] =
    set(getter, setter, (_: M, a: A) => onSet(a), (_: M, a: A) => onRestore(a))(v)

  def set[A](
    getter: M => A,
    setter: A => M => M,
    onSet:  (M, A) => G[Unit]
  )(v:      A): F[Unit] =
    set(getter, setter, onSet, onSet)(v)

  def set[A](
    getter: M => A,
    setter: A => M => M,
    onSet:  A => G[Unit]
  )(v:      A): F[Unit] =
    set(getter, setter, (_: M, a: A) => onSet(a))(v)

  def mod[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => G[Unit],
    onRestore: (M, A) => G[Unit]
  )(f:         A => A): F[Unit]

  def mod[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     A => G[Unit],
    onRestore: A => G[Unit]
  )(f:         A => A): F[Unit] =
    mod(getter, setter, (_: M, a: A) => onSet(a), (_: M, a: A) => onRestore(a))(f)

  def mod[A](
    getter: M => A,
    setter: A => M => M,
    onSet:  (M, A) => G[Unit]
  )(f:      A => A): F[Unit] =
    mod(getter, setter, onSet, onSet)(f)

  def mod[A](
    getter: M => A,
    setter: A => M => M,
    onSet:  A => G[Unit]
  )(f:      A => A): F[Unit] =
    mod(getter, setter, (_: M, a: A) => onSet(a))(f)

  def zoom[N](getN: M => N, modN: (N => N) => (M => M)): UndoSetter[F, G, N] =
    new UndoSetter[F, G, N] {

      override def model: ViewF[F, N]   = self.model.zoom(getN)(modN)
      override val syncToAsync: F ~> G  = self.syncToAsync
      override implicit val F: Monad[F] = self.F

      override def set[A](
        getter:    N => A,
        setter:    A => (N => N),
        onSet:     (N, A) => G[Unit],
        onRestore: (N, A) => G[Unit]
      )(v:         A): F[Unit] =
        self.set(getter.compose(getN),
                 modN.compose(setter),
                 (m, a) => onSet(getN(m), a),
                 (m, a) => onRestore(getN(m), a)
        )(v)

      override def mod[A](
        getter:    N => A,
        setter:    A => (N => N),
        onSet:     (N, A) => G[Unit],
        onRestore: (N, A) => G[Unit]
      )(f:         A => A): F[Unit] =
        self.mod(getter.compose(getN),
                 modN.compose(setter),
                 (m, a) => onSet(getN(m), a),
                 (m, a) => onRestore(getN(m), a)
        )(f)
    }

  def zoom[N](lens: Lens[M, N]): UndoSetter[F, G, N] = zoom(lens.get, lens.modify)

  /**
   * Allows accessing the `UndoSetter` as a `ViewF`.
   *
   * When the `ViewF`'s set/mod is invoked, it will be done through the `UndoSetter`, such that the
   * change is pushed into the undo stack.
   *
   * Using `zoom` into `N` the resulting `ViewF` will still save the whole of `M` in the undo stack.
   * To apply a `zoom` while preserving undo granularity you should `zoom` directly on the
   * `UndoSetter` and create the `undoableView` at the last possible moment.
   */
  def undoableView[N](getN: M => N, modN: (N => N) => M => M): ViewF[F, N] =
    ViewF[F, N](
      model.zoom(getN)(modN).get,
      (f, cb) => mod(getN, (n: N) => modN(_ => n), (n: N) => syncToAsync(cb(n)))(f)
    )

  def undoableView[N](lens: Lens[M, N]): ViewF[F, N] =
    undoableView(lens.get, lens.modify)
}
