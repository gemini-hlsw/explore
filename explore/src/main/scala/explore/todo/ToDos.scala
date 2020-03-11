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
import explore.util.Pot._
import diode.data._
import diode.react.ReactPot._
import react.semanticui.elements.icon.Icon
import react.semanticui.sizes._

final case class ToDos(tasks: Pot[List[Task]]) extends ReactProps {
  @inline def render: VdomElement = ToDos.component(this)
}

object ToDos {
  type Props = ToDos

  case class State(selectedItem: Option[Task] = None, showTodoForm: Boolean = false)

  private implicit val propsReuse: Reusability[Props] = Reusability.derive
  private implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend() {

    val onMount: Callback =
      AppState.actions.todoList.refresh

    def toggle(id: String): IO[Unit] =
      AppState.actions.todoList.toggle(id)

    def render(p: Props) =
      <.div(
        p.tasks.renderPending(_ => Icon(name = "spinner", loading = true, size = Big)),
        p.tasks.renderFailed(_ => <.p("Failed to load")),
        p.tasks.render(tasks => TodoList(tasks, toggle))
      )
  }

  val component = ScalaComponent
    .builder[Props]("ToDos")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
