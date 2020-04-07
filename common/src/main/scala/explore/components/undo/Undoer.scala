// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.undo

import react.common.ReactProps
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import cats.effect.IO
import cats.implicits._
import monocle.macros.Lenses
import crystal.react.implicits._
import monocle.Lens
import monocle.Getter

final case class Undoer[M](
  renderer: Undoer.UndoContext[M] => VdomElement
) extends ReactProps {
  @inline override def render: VdomElement = Undoer.component(this.asInstanceOf[Undoer[Any]])
}

object Undoer {
  case class UndoContext[M](
    set:       Set[M],
    undo:      Undo[M],
    redo:      Redo[M],
    undoEmpty: Boolean,
    redoEmpty: Boolean
  )

  trait Set[M] {
    def apply[A](
      m:      M,
      getter: Getter[M, A],
      setter: A => IO[Unit]
    )(v:      A): IO[Unit]
  }

  type Undo[M] = M => IO[Unit]

  type Redo[M] = M => IO[Unit]

  protected type Props[M] = Undoer[M]

  @Lenses
  protected final case class State[M](
    undoStack: List[Restorer[M]] = List.empty,
    redoStack: List[Restorer[M]] = List.empty
  )

  // implicit protected def propsReuse[M]: Reusability[Props[M]] =
  // Reusability.always
  // implicit protected def stateReuse[M]: Reusability[State[M]] =
  // Reusability.by(s => (s.undoStack.length, s.redoStack.length))

  protected class Backend[M]($ : BackendScope[Props[M], State[M]]) {
    private def push(lens: Lens[State[M], List[Restorer[M]]]): Restorer[M] => IO[Unit] =
      mod => $.modStateIn[IO](lens.modify { stack: List[Restorer[M]] => mod +: stack })

    private def pop(lens: Lens[State[M], List[Restorer[M]]]): IO[Option[Restorer[M]]] =
      $.stateIn[IO].flatMap { s =>
        lens.get(s) match {
          case head :: tail =>
            $.modStateIn[IO](lens.set(tail)).as(head.some)
          case _ =>
            IO(None)
        }
      }

    private def reset(lens: Lens[State[M], List[Restorer[M]]]): IO[Unit] =
      $.modStateIn[IO](lens.set(List.empty))

    private val pushUndo: Restorer[M] => IO[Unit] =
      push(State.undoStack)

    private val pushRedo: Restorer[M] => IO[Unit] =
      push(State.redoStack)

    private val popUndo: IO[Option[Restorer[M]]] =
      pop(State.undoStack)

    private val popRedo: IO[Option[Restorer[M]]] =
      pop(State.redoStack)

    private val resetRedo: IO[Unit] = reset(State.redoStack)

    protected val set: Set[M] = new Set[M] {
      override def apply[A](
        m:      M,
        getter: Getter[M, A],
        setter: A => IO[Unit]
      )(v:      A): IO[Unit] =
        for {
          _ <- pushUndo(Restorer[M, A](m, getter, setter))
          _ <- resetRedo
          _ <- setter(v)
        } yield ()
    }

    // Undo and Redo are "restore" but with switched stacks.
    private def restore(
      popFrom: IO[Option[Restorer[M]]],
      pushTo:  Restorer[M] => IO[Unit]
    )(m:       M): IO[Unit] =
      popFrom.flatMap(_.map(restorer => restorer.restore(m).flatMap(pushTo)).orEmpty)

    protected val undo: Undo[M] =
      restore(popUndo, pushRedo)

    protected val redo: Redo[M] =
      restore(popRedo, pushUndo)

    def render(props: Props[M], state: State[M]): VdomElement =
      // println(s"UNDO STACK: [${state.undoStack}]")
      // println(s"REDO STACK: [${state.redoStack}]")
      props.renderer(UndoContext(set, undo, redo, state.undoStack.isEmpty, state.redoStack.isEmpty))
  }

  protected def componentBuilder[M] =
    ScalaComponent
      .builder[Props[M]]("Undoer")
      .initialState(State[M]())
      .renderBackend[Backend[M]]
      //.configure(Reusability.shouldComponentUpdate)
      .build

  protected val component = componentBuilder[Any]
}
