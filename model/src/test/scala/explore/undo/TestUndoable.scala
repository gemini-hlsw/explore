// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import explore.undo.Undoer
import cats.effect.Sync
import cats.Monoid
import cats.FlatMap
import cats.implicits._
import monocle.macros.Lenses
import cats.effect.concurrent.Ref

@Lenses
case class TestStacks[F[_], A](
  undoStack: Undoer.Stack[F, A] = List.empty,
  redoStack: Undoer.Stack[F, A] = List.empty
)

class TestUndoer[F[_]: Sync, M](stacks: Ref[F, TestStacks[F, M]])(implicit
  monoid:                               Monoid[F[Unit]]
) extends Undoer[F, M] {
  type Stacks = TestStacks[F, M]

  override lazy val getStacks: F[Stacks] = stacks.get

  override def modStacks(mod: Stacks => Stacks): F[Unit] = stacks.modify(s => (mod(s), ()))

  override lazy val undoStack: StackLens = TestStacks.undoStack

  override lazy val redoStack: StackLens = TestStacks.redoStack

  def ctx: F[Undoer.Context[F, M]] =
    stacks.get.map(context)
}

object TestUndoer        {
  def apply[F[_]: Sync, M](implicit monoid: Monoid[F[Unit]]): F[TestUndoer[F, M]] =
    for {
      stacks <- Ref[F].of(TestStacks[F, M]())
    } yield new TestUndoer(stacks)
}

class TestUndoable[F[_]: FlatMap, M](model: Ref[F, M], undoer: TestUndoer[F, M]) {
  def get: F[M] = model.get

  def set[A](getSet: GetSet[M, A], value: A): F[Unit]      =
    for {
      m <- model.get
      c <- undoer.ctx
      _ <- c.setter.set[A](m, getSet.get, (model.update _).compose(getSet.set))(value)
    } yield ()

  def mod[A](getSet: GetSet[M, A], f:     A => A): F[Unit] =
    for {
      m <- model.get
      c <- undoer.ctx
      _ <- c.setter.mod[A](m, getSet.get, (model.update _).compose(getSet.set))(f)
    } yield ()

  def undo: F[Unit] =
    for {
      m <- model.get
      c <- undoer.ctx
      _ <- c.undo(m)
    } yield ()

  def redo: F[Unit] =
    for {
      m <- model.get
      c <- undoer.ctx
      _ <- c.redo(m)
    } yield ()
}

object TestUndoable      {
  def apply[F[_]: Sync, M](
    model:           Ref[F, M]
  )(implicit monoid: Monoid[F[Unit]]): F[TestUndoable[F, M]] =
    for {
      undoer <- TestUndoer[F, M]
    } yield new TestUndoable(model, undoer)
}
