// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.polls

import cats.effect.IO
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.MonocleReact._
import monocle.macros.Lenses
import react.semanticui.elements.button._
import react.semanticui.elements.icon.Icon
import react.semanticui.colors._
import react.semanticui.sizes._
import react.common._
import explore.model.Poll
import explore.model.AppStateIO._
import explore.util.Pot._
import crystal.react.io.implicits._
import java.util.UUID
import diode.data._
import diode.react.ReactPot._

final case class Polls(polls: Pot[List[Poll]]) extends ReactProps {
  @inline def render: VdomElement = Polls.component(this)
}

object Polls {
  type Props = Polls

  @Lenses
  final case class State(casting: Set[UUID] = Set.empty)

  private implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  private implicit val stateReuse: Reusability[State] = Reusability.derive[State]

  class Backend($ : BackendScope[Props, State]) {
    def castingOn(pollId: UUID): IO[Unit] =
      $.modStateL(State.casting)(_ + pollId).toIO

    def castingOff(pollId: UUID): IO[Unit] =
      $.modStateL(State.casting)(_ - pollId).toIO

    def render(props: Props, state: State) =
      <.div(
        props.polls.renderPending(_ => Icon(name = "spinner", loading = true, size = Big)),
        props.polls.renderFailed(_ => <.p("Failed to load")),
        props.polls.render(polls =>
          <.div(
            polls.filter(_.options.nonEmpty).toTagMod { poll =>
              val disabled = state.casting.contains(poll.id)

              <.div(^.key := poll.id.toString)(
                <.h2(poll.question),
                <.div(^.key := "buttons")(
                  poll.options.toTagMod { option =>
                    <.span(^.key := option.id.toString)(
                      Button(
                        color    = Blue,
                        compact  = true,
                        disabled = disabled,
                        onClick = castingOn(poll.id)
                          .flatMap(_ => AppState.Actions.polls.vote(option.id))
                      )(option.text)
                    )
                  },
                  Icon(name = "circle notch", loading = true, size = Large).when(disabled)
                ),
                <.div(^.key := "results",
                      PollResults.component(PollResults(poll.id, castingOff(poll.id))))
              )
            }
          )
        )
      )
  }

  private val component =
    ScalaComponent
      .builder[Props]("Polls")
      .initialState(State())
      .renderBackend[Backend]
      .componentWillMount(_ => AppState.Actions.polls.refresh)
      .configure(Reusability.shouldComponentUpdate)
      .build
}
