// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.Applicative
import cats.syntax.all._
import crystal.implicits._
import crystal.react.View
import crystal.react.implicits._
import japgolly.scalajs.react.util.DefaultEffects.{ Async => DefaultA }
import japgolly.scalajs.react.util.DefaultEffects.{ Sync => DefaultS }
import org.typelevel.log4cats.Logger
import explore.utils.pprinter

/*
 * Combines a view of a model `M` and a view of `UndoStacks` over `M`.
 */
case class UndoContext[M](
  stacks:              View[UndoStacks[DefaultA, M]],
  model:               View[M]
)(implicit val logger: Logger[DefaultA])
    extends UndoSetter[M] {
  private lazy val undoStack: View[UndoStack[DefaultA, M]] = stacks.zoom(UndoStacks.undo)
  private lazy val redoStack: View[UndoStack[DefaultA, M]] = stacks.zoom(UndoStacks.redo)

  lazy val isUndoEmpty: Boolean = stacks.get.undo.isEmpty
  lazy val isRedoEmpty: Boolean = stacks.get.redo.isEmpty

  // Unset "working" on callback passed to react to be executed after setState completion...

  lazy val working: Boolean = stacks.get.working

  private def push(stack: View[UndoStack[DefaultA, M]]): Restorer[DefaultA, M] => DefaultS[Unit] =
    restorer => stack.mod(s => restorer +: s)

  private def undoStacks: DefaultS[Option[Restorer[DefaultA, M]]] =
    stacks.get.undo match {
      case head :: tail =>
        stacks.set(UndoStacks(tail, head.onModel(model.get) +: stacks.get.redo, true)).as(head.some)
      case Nil          => Applicative[DefaultS].pure(none)
    }

  private def redoStacks: DefaultS[Option[Restorer[DefaultA, M]]] =
    stacks.get.redo match {
      case head :: tail =>
        stacks.set(UndoStacks(head.onModel(model.get) +: stacks.get.undo, tail, true)).as(head.some)
      case Nil          => Applicative[DefaultS].pure(none)
    }

  private def reset(stack: View[UndoStack[DefaultA, M]]): DefaultS[Unit] =
    stack.set(List.empty)

  def restore(restorerOpt: Option[Restorer[DefaultA, M]]): DefaultS[Unit] =
    restorerOpt
      .map(restorer =>
        for {
          _ <- model.mod(restorer.setter(restorer.value))
          f <-
            (restorer.onRestore(model.get, restorer.value) >>
              // Set working to false after returning from side effect.
              stacks
                .zoom(UndoStacks.working[DefaultA, M])
                .set(false)
                .to[DefaultA]).runAsyncAndForget
        } yield f
      )
      .orUnit

  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => DefaultA[Unit],
    onRestore: (M, A) => DefaultA[Unit]
  )(v:         A): DefaultS[Unit] =
    for {
      _ <- push(undoStack)(Restorer[DefaultA, M, A](model.get, getter, setter, onRestore))
      _ <- reset(redoStack)
      _ <- model.mod.compose(setter)(v)
      f <- onSet(model.get, v).runAsync
    } yield f

  def mod[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => DefaultA[Unit],
    onRestore: (M, A) => DefaultA[Unit]
  )(f:         A => A): DefaultS[Unit] =
    set(getter, setter, onSet, onRestore)(f(getter(model.get)))

  val undo: DefaultS[Unit] = undoStacks >>= restore

  val redo: DefaultS[Unit] = redoStacks >>= restore
}
