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
import cats.effect.Async
import cats.effect.Sync
import cats.kernel.Monoid

final case class Undoer[M](
  renderer: Undoer.UndoContext[IO, M] => VdomElement
) extends Undoer.Props[IO, M]
    with ReactProps {
  @inline override def render: VdomElement =
    Undoer.component(this.asInstanceOf[Undoer.Props[IO, Any]])
}

object Undoer {
  case class UndoContext[F[_], M](
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

  protected trait Props[F[_], M] {
    val renderer: UndoContext[F, M] => VdomElement
  }

  @Lenses
  protected final case class State[F[_], M](
    undoStack: List[Restorer[F, M]] = List.empty,
    redoStack: List[Restorer[F, M]] = List.empty
  )

  implicit protected def propsReuse[F[_], M]: Reusability[Props[F, M]] =
    Reusability.always
  implicit protected def stateReuse[F[_], M]: Reusability[State[F, M]] =
    Reusability.never

  protected class Backend[F[_]: Async, M]($ : BackendScope[Props[F, M], State[F, M]])(
    implicit monoid:                         Monoid[F[Unit]]
  ) {
    private def push(lens: Lens[State[F, M], List[Restorer[F, M]]]): Restorer[F, M] => F[Unit] =
      mod => $.modStateIn[F](lens.modify { stack: List[Restorer[F, M]] => mod +: stack })

    private def pop(lens: Lens[State[F, M], List[Restorer[F, M]]]): F[Option[Restorer[F, M]]] =
      $.stateIn[F].flatMap { s =>
        lens.get(s) match {
          case head :: tail =>
            $.modStateIn[F](lens.set(tail)).as(head.some)
          case _ =>
            Sync[F].delay(None)
        }
      }

    private def reset(lens: Lens[State[F, M], List[Restorer[F, M]]]): F[Unit] =
      $.modStateIn[F](lens.set(List.empty))

    private val pushUndo: Restorer[F, M] => F[Unit] =
      push(State.undoStack)

    private val pushRedo: Restorer[F, M] => F[Unit] =
      push(State.redoStack)

    private val popUndo: F[Option[Restorer[F, M]]] =
      pop(State.undoStack)

    private val popRedo: F[Option[Restorer[F, M]]] =
      pop(State.redoStack)

    private val resetRedo: F[Unit] = reset(State.redoStack)

    protected val set: Set[F, M] = new Set[F, M] {
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

    protected val undo: Undo[F, M] =
      restore(popUndo, pushRedo)

    protected val redo: Redo[F, M] =
      restore(popRedo, pushUndo)

    def render(props: Props[F, M], state: State[F, M]): VdomElement =
      // println(s"UNDO STACK: [${state.undoStack}]")
      // println(s"REDO STACK: [${state.redoStack}]")
      props.renderer(UndoContext(set, undo, redo, state.undoStack.isEmpty, state.redoStack.isEmpty))
  }

  protected def componentBuilder[M] =
    ScalaComponent
      .builder[Props[IO, M]]("Undoer")
      .initialState(State[IO, M]())
      .renderBackend[Backend[IO, M]]
      .configure(Reusability.shouldComponentUpdate)
      .build

  protected val component = componentBuilder[Any]
}
