// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.undo

import cats.effect.Async
import cats.effect.IO
import crystal.react.implicits._
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import react.common.ReactProps
import explore.utils.reuse._

final case class UndoRegion[M](
  renderer: Undoer.Context[IO, M] ==> VdomNode
) extends ReactProps(UndoRegion.component)
    with UndoRegion.Props[IO, M]

object UndoRegion {
  protected trait Props[F[_], M] {
    val renderer: Undoer.Context[F, M] ==> VdomNode
  }

  @Lenses
  protected final case class State[F[_], M](
    undoStack: Undoer.Stack[F, M] = List.empty,
    redoStack: Undoer.Stack[F, M] = List.empty
  )

  // Reusability should be controlled by enclosing components and reuse parameter. We allow rerender every time it's requested.
  implicit protected def propsReuse[F[_], M]: Reusability[Props[F, M]] =
    Reusability.never
  // Internal changes in the stacks do not trigger rerenders.
  implicit protected def stateReuse[F[_], M]: Reusability[State[F, M]] =
    Reusability.always

  protected class Backend[F[_]: Async, M]($ : BackendScope[Props[F, M], State[F, M]])
      extends Undoer[F, M] {
    type Stacks = State[F, M]

    override lazy val getStacks: F[Stacks] = $.stateIn[F]

    override def modStacks(mod: Stacks => Stacks): F[Unit] = $.modStateIn[F](mod)

    override lazy val undoStack = State.undoStack

    override lazy val redoStack = State.redoStack

    def render(props: Props[F, M], state: State[F, M]): VdomNode =
      // println(s"UNDO STACK: [${state.undoStack}]")
      // println(s"REDO STACK: [${state.redoStack}]")
      props.renderer(context(state))
  }

  protected def componentBuilder[M] =
    ScalaComponent
      .builder[Props[IO, M]]
      .initialState(State[IO, M]())
      .renderBackend[Backend[IO, M]]
      .configure(Reusability.shouldComponentUpdate)
      .build

  val component = componentBuilder[Any]
}
