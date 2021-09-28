// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.Applicative
import cats.FlatMap
import cats.Monad
import cats.effect.std.Dispatcher
import cats.syntax.all._
import cats.~>
import crystal.ViewF
import crystal.implicits._

/*
 * Combines a view of a model `M` and a view of `UndoStacks` over `M`.
 */
case class UndoContext[F[_], G[_], M](
  stacks:         ViewF[F, UndoStacks[G, M]],
  model:          ViewF[F, M]
)(implicit val F: Monad[F], G: FlatMap[G], dispatcher: Dispatcher[G], val syncToAsync: F ~> G)
    extends UndoSetter[F, G, M] {
  private lazy val undoStack: ViewF[F, UndoStack[G, M]] = stacks.zoom(UndoStacks.undo)
  private lazy val redoStack: ViewF[F, UndoStack[G, M]] = stacks.zoom(UndoStacks.redo)

  lazy val isUndoEmpty: Boolean = stacks.get.undo.isEmpty
  lazy val isRedoEmpty: Boolean = stacks.get.redo.isEmpty

  // Unset "working" on callback passed to react to be executed after setState completion...

  lazy val working: Boolean = stacks.get.working

  private def push(stack: ViewF[F, UndoStack[G, M]]): Restorer[G, M] => F[Unit] =
    restorer => stack.mod(s => restorer +: s)

  private def undoStacks: F[Option[Restorer[G, M]]] =
    stacks.get.undo match {
      case head :: tail =>
        stacks.set(UndoStacks(tail, head.onModel(model.get) +: stacks.get.redo, true)).as(head.some)
      case Nil => Applicative[F].pure(none)
    }

  private def redoStacks: F[Option[Restorer[G, M]]] =
    stacks.get.redo match {
      case head :: tail =>
        stacks.set(UndoStacks(head.onModel(model.get) +: stacks.get.undo, tail, true)).as(head.some)
      case Nil => Applicative[F].pure(none)
    }

  private def reset(stack: ViewF[F, UndoStack[G, M]]): F[Unit] =
    stack.set(List.empty)

  def restore(restorerOpt: Option[Restorer[G, M]]): F[Unit] =
    restorerOpt
      .map(restorer =>
        for {
          _ <- model.mod(restorer.setter(restorer.value))
        } yield dispatcher.unsafeRunAndForget(
          restorer.onRestore(model.get, restorer.value) >>
            // Set working to false after returning from side effect.
            syncToAsync(stacks.zoom(UndoStacks.working[G, M]).set(false))
        )
      )
      .orUnit

  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => G[Unit],
    onRestore: (M, A) => G[Unit]
  )(v:         A): F[Unit] =
    for {
      _ <- push(undoStack)(Restorer[G, M, A](model.get, getter, setter, onRestore))
      _ <- reset(redoStack)
      _ <- model.mod.compose(setter)(v)
    } yield dispatcher.unsafeRunAndForget(onSet(model.get, v))

  def mod[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => G[Unit],
    onRestore: (M, A) => G[Unit]
  )(f:         A => A): F[Unit] =
    set(getter, setter, onSet, onRestore)(f(getter(model.get)))

  val undo: F[Unit] = undoStacks >>= restore

  val redo: F[Unit] = redoStacks >>= restore
}
