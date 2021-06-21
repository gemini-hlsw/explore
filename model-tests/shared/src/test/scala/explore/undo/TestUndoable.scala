// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.~>
import cats.effect.Ref
import cats.effect.Concurrent
import cats.syntax.all._
import explore.optics.GetAdjust
import cats.effect.std.Dispatcher
import cats.Monad
import crystal.ViewF
import cats.arrow.FunctionK
import crystal.refModCB
import cats.Applicative

class TestUndoable[F[_]: Monad, M](
  val valueRef:            Ref[F, M],
  stacksRef:               Ref[F, UndoStacks[F, M]]
)(implicit val dispatcher: Dispatcher[F]) {
  def get: F[M] = valueRef.get

  def set[A](getAdjust: GetAdjust[M, A], v: A): F[Unit]      =
    context >>= (_.set(getAdjust.get, getAdjust.set, (_: A) => Applicative[F].unit)(v))

  def mod[A](getAdjust: GetAdjust[M, A], f: A => A): F[Unit] =
    context >>= (_.mod(getAdjust.get, getAdjust.set, (_: A) => Applicative[F].unit)(f))

  def undo: F[Unit] = context >>= (_.undo)

  def redo: F[Unit] = context >>= (_.redo)

  private def refView[A](ref: Ref[F, A]): F[ViewF[F, A]] =
    ref.get.map(a => ViewF(a, refModCB(ref)))

  implicit private val syncToAsync: F ~> F = FunctionK.id[F]

  val context: F[UndoContext[F, F, M]] =
    for {
      valueView  <- refView(valueRef)
      stacksView <- refView(stacksRef)
    } yield UndoContext(stacksView, valueView)

}

object TestUndoable {
  def apply[F[_]: Concurrent, M](
    initValue:           M
  )(implicit dispatcher: Dispatcher[F]): F[TestUndoable[F, M]] =
    for {
      valueRef  <- Ref[F].of(initValue)
      stacksRef <- Ref[F].of(UndoStacks.empty[F, M])
    } yield new TestUndoable(valueRef, stacksRef)
}
