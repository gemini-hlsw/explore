// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.todo

import explore.model.Task
import cats.effect.IO
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps
import crystal.react.io.implicits._

final case class TodoList(
  items:  List[Task],
  toggle: String => IO[Unit]
) extends ReactProps {
  @inline def render: VdomElement = TodoList.component(this)
}

object TodoList {
  type Props = TodoList

  val component = ScalaComponent
    .builder[Props]("TodoList")
    .render_P { p =>
      def renderItem(item: Task) =
        <.li(
          <.input.checkbox(^.checked := item.completed, ^.onChange --> p.toggle(item.id)),
          <.span(" "),
          if (item.completed) <.s(item.title) else <.span(item.title)
        )

      <.ul(p.items.toTagMod(renderItem))
    }
    .build
}
