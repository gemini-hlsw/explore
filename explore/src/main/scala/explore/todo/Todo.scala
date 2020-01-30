package explore.todo
/*
import diode.react.ReactPot._
import diode.data.Pot
import spatutorial.client.components.Bootstrap._
import spatutorial.client.components._
import spatutorial.client.logger._
import spatutorial.client.services._
import spatutorial.shared._
import scalacss.ScalaCssReact._
import spatutorial.client.services.Algebras._

final case class Todo(view: View[IO, Pot[Todos]]) extends ReactProps {
  @inline def render: VdomElement = Todo.component(this)
}
 */

import cats.effect.IO
import react.common.ReactProps
import crystal._
import crystal.react.io.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import explore.model.Task
import explore.model.TodoListActions
import explore.model.Actions._

final case class Todo(view: View[IO, List[Task]]) extends ReactProps {
  @inline def render: VdomElement = Todo.component(this)
}

object Todo {
  type Props = Todo

  case class State(selectedItem: Option[Task] = None, showTodoForm: Boolean = false)

  class Backend( /*$: BackendScope[Props, State]*/ ) {
    //   println($)

    private def actions(props: Props) = props.view.actions[TodoListActions]

    def mounted(props: Props) =
      // dispatch a message to refresh the todos, which will cause TodoStore to fetch todos from the server
      actions(props).refresh().when(props.view.get.map(_.isEmpty))

    def toggle(props: Props)(id: String): IO[Unit] =
      for {
        _ <- actions(props).toggle(id)
        _ <- actions(props).refresh()
      } yield ()

    /*
    def editTodo(item: Option[TodoItem]) =
    // activate the edit dialog
      $.modStateIO(s => s.copy(selectedItem = item, showTodoForm = true))

    def todoEdited(item: TodoItem, cancelled: Boolean): IO[Unit] = {
      val io = if (cancelled) {
        for {
          p <- $.propsIO
          _ <- p.view.algebra[LogAlgebra].log("Todo editing cancelled")
        } yield ()
      } else {
        for {
          p <- $.propsIO
          _ <- p.view.algebra[LogAlgebra].log(s"Todo edited: $item")
          _ <- p.view.algebra[TodosAlgebra].updateTodo(item)
        } yield ()
      }
      // hide the edit dialog, chain IOs
      io.flatMap(_ => $.modStateIO(s => s.copy(showTodoForm = false)))
    }*/

    def render(p: Props /*, s: State*/ ) =
      <.div(
        p.view.streamRender { tasks =>
          TodoList(tasks, toggle(p))
        }
      )
    /*
      Panel("What needs to be done")(
        p.view.flow { todosOpt =>
          val todos = Pot.fromOption(todosOpt).flatten
          <.div(
            todos.renderFailed(ex => "Error loading"),
            todos.renderPending(_ > 500, _ => "Loading..."),
            todos.render(todos => TodoList(todos.items,
              item => p.view.algebra[TodosAlgebra].updateTodo(item),
              item => editTodo(Some(item)),
              item => p.view.algebra[TodosAlgebra].deleteTodo(item))),
            Button(editTodo(None))(Icon.plusSquare, " New")
          )
        },


        // if the dialog is open, add it to the panel
        if (s.showTodoForm) TodoForm(s.selectedItem, todoEdited)
        else // otherwise add an empty placeholder
          VdomArray.empty())
   */
  }

  // create the React component for To Do management
  val component = ScalaComponent
    .builder[Props]("Todo")
    .initialState(State()) // initial state from TodoStore
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build
}

/*
final case class TodoForm(
                           item: Option[TodoItem],
                           submitHandler: (TodoItem, Boolean) => IO[Unit]
                         ) extends ReactProps {
  @inline def render: VdomElement = TodoForm.component(this)
}

object TodoForm {
  // shorthand for styles
  @inline private def bss = GlobalStyles.bootstrapStyles

  type Props = TodoForm

  case class State(item: TodoItem, cancelled: Boolean = true)

  class Backend(t: BackendScope[Props, State]) {
    def submitForm(): IO[Unit] = {
      // mark it as NOT cancelled (which is the default)
      t.modStateIO(s => s.copy(cancelled = false))
    }

    def formClosed(state: State, props: Props): IO[Unit] =
    // call parent handler with the new item and whether form was OK or cancelled
      props.submitHandler(state.item, state.cancelled)

    def updateDescription(e: ReactEventFromInput): IO[Unit] = {
      val text = e.target.value
      // update TodoItem content
      t.modStateIO(s => s.copy(item = s.item.copy(content = text)))
    }

    def updatePriority(e: ReactEventFromInput): IO[Unit] = {
      // update TodoItem priority
      val newPri = e.currentTarget.value match {
        case p if p == TodoHigh.toString => TodoHigh
        case p if p == TodoNormal.toString => TodoNormal
        case p if p == TodoLow.toString => TodoLow
      }
      t.modStateIO(s => s.copy(item = s.item.copy(priority = newPri)))
    }

    def render(p: Props, s: State) = {
      log.debug(s"User is ${if (s.item.id == "") "adding" else "editing"} a todo or two")
      val headerText = if (s.item.id == "") "Add new todo" else "Edit todo"
      Modal(
        // header contains a cancel button (X)
        header = hide => <.span(<.button(^.tpe := "button", bss.close, ^.onClick --> hide, Icon.close), <.h4(headerText)),
        // footer has the OK button that submits the form before hiding it
        footer = hide => <.span(Button(submitForm() >> hide)("OK")),
        // this is called after the modal has been hidden (animation is completed)
        closed = formClosed(s, p)
      )(
        <.div(bss.formGroup,
          <.label(^.`for` := "description", "Description"),
          <.input.text(bss.formControl, ^.id := "description", ^.value := s.item.content,
            ^.placeholder := "write description", ^.onChange ==> updateDescription)),
        <.div(bss.formGroup,
          <.label(^.`for` := "priority", "Priority"),
          // using defaultValue = "Normal" instead of option/selected due to React
          <.select(bss.formControl, ^.id := "priority", ^.value := s.item.priority.toString, ^.onChange ==> updatePriority,
            <.option(^.value := TodoHigh.toString, "High"),
            <.option(^.value := TodoNormal.toString, "Normal"),
            <.option(^.value := TodoLow.toString, "Low")
          )
        )
      )
    }
  }

  val component = ScalaComponent.builder[Props]("TodoForm")
    .initialStateFromProps(p => State(p.item.getOrElse(TodoItem("", 0, "", TodoNormal, completed = false))))
    .renderBackend[Backend]
    .build
}
 */
