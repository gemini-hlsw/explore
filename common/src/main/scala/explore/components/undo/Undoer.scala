// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.undo

import monocle.Getter
import monocle.Lens
import cats.FlatMap
import cats.implicits._
import cats.effect.Sync
import cats.Monoid

object Undoer {
  type Stack[F[_], M] = List[Restorer[F, M]]

  case class Context[F[_], M](
    set:       Set[F, M],
    undo:      Undo[F, M],
    redo:      Redo[F, M],
    undoEmpty: Boolean,
    redoEmpty: Boolean
  )

  trait Set[F[_], M] {
    def apply[A](
      m:      M,
      getter: Getter[M, A],
      setter: A => F[Unit]
    )(v:      A): F[Unit]
  }

  type Undo[F[_], M] = M => F[Unit]

  type Redo[F[_], M] = M => F[Unit]
}

abstract class Undoer[F[_]: Sync, M](implicit monoid: Monoid[F[Unit]]) {
  type Stacks

  val getStacks: F[Stacks]

  def modStacks(mod: Stacks => Stacks): F[Unit]

  type StackLens = Lens[Stacks, Undoer.Stack[F, M]]

  val undoStack: StackLens

  val redoStack: StackLens

  private def push(lens: Lens[Stacks, Undoer.Stack[F, M]]): Restorer[F, M] => F[Unit] =
    mod => modStacks(lens.modify { stack: List[Restorer[F, M]] => mod +: stack })

  private def pop(lens: Lens[Stacks, Undoer.Stack[F, M]]): F[Option[Restorer[F, M]]] =
    FlatMap[F].flatMap(getStacks) { s =>
      lens.get(s) match {
        case head :: tail =>
          modStacks(lens.set(tail)).as(head.some)
        case _ =>
          Sync[F].delay(None)
      }
    }

  private def reset(lens: Lens[Stacks, List[Restorer[F, M]]]): F[Unit] =
    modStacks(lens.set(List.empty))

  private val pushUndo: Restorer[F, M] => F[Unit] =
    push(undoStack)

  private val pushRedo: Restorer[F, M] => F[Unit] =
    push(redoStack)

  private val popUndo: F[Option[Restorer[F, M]]] =
    pop(undoStack)

  private val popRedo: F[Option[Restorer[F, M]]] =
    pop(redoStack)

  private val resetRedo: F[Unit] = reset(redoStack)

  protected val set: Undoer.Set[F, M] = new Undoer.Set[F, M] {
    override def apply[A](
      m:      M,
      getter: Getter[M, A],
      setter: A => F[Unit]
    )(v:      A): F[Unit] =
      for {
        _ <- pushUndo(Restorer[F, M, A](m, getter, setter))
        _ <- resetRedo
        _ <- setter(v)
      } yield ()
  }

  // Undo and Redo are "restore" but with switched stacks.
  private def restore(
    popFrom: F[Option[Restorer[F, M]]],
    pushTo:  Restorer[F, M] => F[Unit]
  )(m:       M): F[Unit] =
    popFrom.flatMap(_.map(restorer => restorer.restore(m).flatMap(pushTo)).orEmpty)

  protected val undo: Undoer.Undo[F, M] =
    restore(popUndo, pushRedo)

  protected val redo: Undoer.Redo[F, M] =
    restore(popRedo, pushUndo)

  protected def context(stacks: Stacks): Undoer.Context[F, M] =
    Undoer.Context(set, undo, redo, undoStack.get(stacks).isEmpty, redoStack.get(stacks).isEmpty)
}
