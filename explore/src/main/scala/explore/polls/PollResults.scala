// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.polls

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.elements.icon.Icon
import react.semanticui.sizes._
import java.util.UUID
import explore.model.AppStateIO._
import cats.implicits._
import cats.effect.IO
import crystal._
import crystal.react.StreamRenderer
import crystal.react.io.implicits._
import explore.graphql.polls.PollResultsSubscription
import clue.js.WebSocketGraphQLClient
import diode.data._
import diode.react.ReactPot._

final case class PollResults(pollId: UUID, onNewData: IO[Unit]) extends ReactProps {
  @inline def render: VdomElement = PollResults.component(this)
}

object PollResults {
  type Props = PollResults

  type Results = List[PollResultsSubscription.PollResult]

  final case class State(
    subscription: Option[
      WebSocketGraphQLClient[IO]#ApolloSubscription[PollResultsSubscription.Data]
    ] = None,
    renderer: Pot[StreamRenderer[Results]] = Pending()
  )

  implicit val propsReuse: Reusability[Props] = Reusability.always
  implicit val stateReuse: Reusability[State] = Reusability.never

  val component =
    ScalaComponent
      .builder[Props]("PollResults")
      .initialState(State())
      .render_S { state =>
        <.div(
          state.renderer.renderPending(_ => Icon(name = "spinner", loading = true, size = Large)),
          state.renderer.render(
            _ { results =>
              <.ol(
                results.toTagMod { result =>
                  (for {
                    option <- result.option
                    votes  <- result.votes
                  } yield (option.text, votes)).whenDefined {
                    case (text, votes) =>
                      <.li(^.key := result.option.map(_.id.toString).orEmpty, s"$text: $votes")
                  }
                }
              )
            }
          )
        )
      }
      .componentWillMount { $ =>
        AppState.Clients.polls
          .subscribe(PollResultsSubscription)(
            PollResultsSubscription.Variables($.props.pollId).some
          )
          .flatMap { subscription =>
            $.modStateIO(_ =>
              State(subscription.some,
                    Ready(
                      StreamRenderer
                        .build(
                          subscription.stream
                            .map(_.poll_results)
                            .flatTap(_ => fs2.Stream.eval($.props.onNewData))
                        )
                    ))
            )
          }
      }
      .componentWillUnmount($ => $.state.subscription.map(_.stop).orEmpty)
      .configure(Reusability.shouldComponentUpdate)
      .build
}
