package explore.polls

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.elements.button._
import react.common._
import explore.model.Poll
import explore.model.Actions.PollsActionsIO
import crystal.react.io.implicits._
import cats.effect._

final case class Polls(polls: List[Poll]) extends ReactProps {
  @inline def render: VdomElement = Polls.component(this)
}

object Polls {
  type Props = Polls

  final case class State(casting: Boolean = false)

  private val component =
    ScalaComponent
      .builder[Props]("Polls")
      .initialState(State())
      .render { $ =>
        <.div(
          $.props.polls.toTagMod { poll =>
            <.div(
              <.h2(poll.question),
              poll.options.toTagMod { option =>
                <.span(
                  Button(onClick = /*$.setState(State(true)).toIO
                      .flatMap(_ => */ PollsActionsIO.vote(option.id))(option.text)
                )
              },
              // <.span("CASTING...").when($.state.casting),
              <.div(PollResults(poll.id, IO.unit /*$.setState(State(false)).toIO*/ ))
            )
          }
        )
      }
      .componentWillMount { _ =>
        PollsActionsIO.refresh()
      }
      .build
}
