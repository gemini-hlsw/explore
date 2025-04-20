// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.Applicative
import cats.syntax.all.*
import crystal.react.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA
import japgolly.scalajs.react.util.DefaultEffects.Sync as DefaultS

/*
 * Combines a view of a model `M` and a view of `UndoStacks` over `M`.
 */
case class UndoContext[M](
  stacks: View[UndoStacks[DefaultA, M]],
  model:  View[M]
) extends UndoSetter[M]
    with Undoer {

  private lazy val undoStack: View[UndoStack[DefaultA, M]] = stacks.zoom(UndoStacks.undo)
  private lazy val redoStack: View[UndoStack[DefaultA, M]] = stacks.zoom(UndoStacks.redo)

  lazy val isUndoEmpty: Boolean = stacks.get.undo.isEmpty
  lazy val isRedoEmpty: Boolean = stacks.get.redo.isEmpty

  // Unset "working" on callback passed to react to be executed after setState completion...

  lazy val working: Boolean = stacks.get.working

  private def push(stack: View[UndoStack[DefaultA, M]]): Restorer[DefaultA, M] => DefaultS[Unit] =
    restorer => stack.mod(s => restorer +: s)

  // Move the top of the undo stack to the redo stack and return it.
  private def popUndoStack: DefaultS[Option[Restorer[DefaultA, M]]] =
    DefaultS
      .delay(stacks.get.undo)
      .flatMap:
        case head :: tail =>
          stacks
            .set(UndoStacks(tail, head.onModel(model.get) +: stacks.get.redo, true))
            .as(head.some)
        case Nil          => Applicative[DefaultS].pure(none)

  // Move the top of the redo stack to the undo stack and return it.
  private def popRedoStack: DefaultS[Option[Restorer[DefaultA, M]]] =
    DefaultS
      .delay(stacks.get.redo)
      .flatMap:
        case head :: tail =>
          stacks
            .set(UndoStacks(head.onModel(model.get) +: stacks.get.undo, tail, true))
            .as(head.some)
        case Nil          => Applicative[DefaultS].pure(none)

  private def reset(stack: View[UndoStack[DefaultA, M]]): DefaultS[Unit] =
    stack.set(List.empty)

  def restore(restorerOpt: Option[Restorer[DefaultA, M]]): DefaultS[Unit] =
    restorerOpt
      .map: restorer =>
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
      .getOrEmpty

  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => DefaultA[Unit], // M is the old model, A is the new value
    onRestore: (M, A) => DefaultA[Unit]  // M is the old model, A is the new value
  )(v: A): DefaultS[Unit] =
    mod(getter, setter, onSet, onRestore)(_ => v)

  def mod[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => DefaultA[Unit], // M is the old model, A is the new value
    onRestore: (M, A) => DefaultA[Unit]  // M is the old model, A is the new value
  )(f: A => A): DefaultS[Unit] =
    model.modCB(
      oldModel => setter(f(getter(oldModel)))(oldModel),
      (oldModel, newModel) =>
        push(undoStack)(Restorer[DefaultA, M, A](oldModel, getter, setter, onRestore)) >>
          reset(redoStack) >>
          onSet(oldModel, getter(newModel)).runAsyncAndForget
    )

  val undo: DefaultS[Unit] = popUndoStack >>= restore

  val redo: DefaultS[Unit] = popRedoStack >>= restore
}
