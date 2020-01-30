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
//   editItem: TodoItem => IO[Unit],
//   deleteItem: TodoItem => IO[Unit]
) extends ReactProps {
  @inline def render: VdomElement = TodoList.component(this)
}

object TodoList {
  type Props = TodoList

  // shorthand for styles
//   @inline private def bss = GlobalStyles.bootstrapStyles

  val component = ScalaComponent
    .builder[Props]("TodoList")
    .render_P { p =>
      //   val style = bss.listGroup
      def renderItem(item: Task) =
        // convert priority into Bootstrap style
        // val itemStyle = item.priority match {
        //   case TodoLow => style.itemOpt(CommonStyle.info)
        //   case TodoNormal => style.item
        //   case TodoHigh => style.itemOpt(CommonStyle.danger)
        // }
        <.li(
          /*itemStyle,*/
          <.input.checkbox(^.checked := item.completed, ^.onChange --> p.toggle(item.id)),
          <.span(" "),
          if (item.completed) <.s(item.title) else <.span(item.title)
          //   Button(p.editItem(item), addStyles = Seq(bss.pullRight, bss.buttonXS))("Edit"),
          //   Button(p.deleteItem(item), addStyles = Seq(bss.pullRight, bss.buttonXS))("Delete")
        )

      <.ul /*(style.listGroup)*/ (p.items.toTagMod(renderItem))
    }
    .build
}
