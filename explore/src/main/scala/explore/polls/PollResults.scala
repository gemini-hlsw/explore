// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.polls

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import java.util.UUID
import explore.model.AppStateIO._
import cats.implicits._
import cats.effect.IO
import explore.graphql.polls.PollResultsSubscription
import explore.components.graphql.SubscriptionRender

final case class PollResults(pollId: UUID, onNewData: IO[Unit]) extends ReactProps {
  @inline def render: VdomElement = PollResults.component(this)
}

object PollResults {
  type Props = PollResults

  implicit val propsReuse: Reusability[Props] = Reusability.always

  val component =
    ScalaComponent
      .builder[Props]("PollResults")
      .render { $ =>
        SubscriptionRender[PollResultsSubscription.Data, List[PollResultsSubscription.PollResult]](
          AppState.clients.polls
            .subscribe(PollResultsSubscription)(
              PollResultsSubscription.Variables($.props.pollId).some
            ),
          _.map(_.poll_results)
        )(
          pollResults =>
            <.ol(
              pollResults.toTagMod { result =>
                (for {
                  option <- result.option
                  votes  <- result.votes
                } yield (option.text, votes)).whenDefined {
                  case (text, votes) =>
                    <.li(^.key := result.option.map(_.id.toString).orEmpty, s"$text: $votes")
                }
              }
            ),
          $.props.onNewData
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
