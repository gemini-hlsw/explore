// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.todo

import cats.effect.IO
import react.common.ReactProps
import crystal.react.io.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import explore.model.Task
import explore.model.AppStateIO._

final case class Todo(tasks: List[Task]) extends ReactProps {
  @inline def render: VdomElement = Todo.component(this)
}

object Todo {
  type Props = Todo

  case class State(selectedItem: Option[Task] = None, showTodoForm: Boolean = false)

  private implicit val propsReuse: Reusability[Props] = Reusability.derive
  private implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend() {

    def onMount(): Callback =
      AppState.Actions.todoList.refresh()

    def toggle(id: String): IO[Unit] =
      for {
        _ <- AppState.Actions.todoList.toggle(id)
        _ <- AppState.Actions.todoList.refresh()
      } yield ()

    def render(p: Props) =
      try {
        <.div(
          <.b("TASKS:"),
          TodoList(p.tasks, toggle)
        )
      } catch {
        case e: Throwable =>
          e.printStackTrace()
          <.div("ERROR")
      }
  }

  val component = ScalaComponent
    .builder[Props]("Todo")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.onMount())
    .configure(Reusability.shouldComponentUpdate)
    .build
}
