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
import cats.effect.Async
import cats.kernel.Monoid

final case class UndoRegion[M](
  renderer: Undoer.Context[IO, M] => VdomElement
) extends UndoRegion.Props[IO, M]
    with ReactProps {
  @inline override def render: VdomElement =
    UndoRegion.component(this.asInstanceOf[UndoRegion.Props[IO, Any]])
}

object UndoRegion {
  protected trait Props[F[_], M] {
    val renderer: Undoer.Context[F, M] => VdomElement
  }

  @Lenses
  protected final case class State[F[_], M](
    undoStack: Undoer.Stack[F, M] = List.empty,
    redoStack: Undoer.Stack[F, M] = List.empty
  )

  implicit protected def propsReuse[F[_], M]: Reusability[Props[F, M]] =
    Reusability.always
  implicit protected def stateReuse[F[_], M]: Reusability[State[F, M]] =
    Reusability.never

  protected class Backend[F[_]: Async, M]($ : BackendScope[Props[F, M], State[F, M]])(
    implicit monoid:                         Monoid[F[Unit]]
  ) extends Undoer[F, M] {
    type Stacks = State[F, M]

    override lazy val getStacks: F[Stacks] = $.stateIn[F]

    override def modStacks(mod: Stacks => Stacks): F[Unit] = $.modStateIn[F](mod)

    override lazy val undoStack = State.undoStack

    override lazy val redoStack = State.redoStack

    def render(props: Props[F, M], state: State[F, M]): VdomElement =
      // println(s"UNDO STACK: [${state.undoStack}]")
      // println(s"REDO STACK: [${state.redoStack}]")
      props.renderer(context(state))
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
