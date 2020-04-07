// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.todo

import explore.implicits._
import cats.effect.IO
import react.common.ReactProps
import crystal.react.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import explore.model.Task
import explore.util.Pot._
import diode.data._
import diode.react.ReactPot._
import react.semanticui.elements.icon.Icon
import react.semanticui.sizes._

final case class ToDos(tasks: ViewCtxF[Pot[List[Task]]]) extends ReactProps {
  @inline def render: VdomElement = ToDos.component(this)
}

object ToDos {
  type Props = ToDos

  case class State(selectedItem: Option[Task] = None, showTodoForm: Boolean = false)

  private implicit val propsReuse: Reusability[Props] = Reusability.derive
  private implicit val stateReuse: Reusability[State] = Reusability.derive

  def onMount(p: Props): IO[Unit] =
    p.tasks.actions(_.todoList).refresh

  class Backend() {
    def render(p: Props) = {
      def toggle(id: String): IO[Unit] =
        p.tasks.actions(_.todoList).toggle(id)

      <.div(
        p.tasks.get.renderPending(_ => Icon(name = "spinner", loading = true, size = Big)),
        p.tasks.get.renderFailed(_ => <.p("Failed to load")),
        p.tasks.get.render(tasks => TodoList(tasks, toggle))
      )
    }
  }

  val component = ScalaComponent
    .builder[Props]("ToDos")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount($ => onMount($.props).toCB)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
