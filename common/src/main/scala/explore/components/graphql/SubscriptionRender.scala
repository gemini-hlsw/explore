// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.graphql

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.elements.icon.Icon
import react.semanticui.sizes._
import clue.GraphQLStreamingClient
import cats.implicits._
import cats.effect.IO
import crystal._
import crystal.react.StreamRenderer
import crystal.react.io.implicits._
import diode.data._
import diode.react.ReactPot._
import cats.effect.ContextShift

final case class SubscriptionRender[D](
  subscribe: IO[GraphQLStreamingClient[IO]#Subscription[D]]
)(
  val valueRender: D => VdomNode,
  val onNewData:   IO[Unit] = IO.unit
)(
  implicit val cs: ContextShift[IO]
) extends ReactProps {
  override def render: VdomElement =
    SubscriptionRender.component(this.asInstanceOf[SubscriptionRender[Any]])
}

object SubscriptionRender {
  type Props[D] = SubscriptionRender[D]

  final case class State[D](
    subscription: Option[
      GraphQLStreamingClient[IO]#Subscription[D]
    ] = None,
    renderer: Option[StreamRenderer[Pot[D]]] = None
  )

  implicit def propsReuse[D]: Reusability[Props[D]] = Reusability.always
  implicit def stateReuse[D]: Reusability[State[D]] = Reusability.never

  protected def componentBuilder[D] =
    ScalaComponent
      .builder[Props[D]]("PollResults")
      .initialState(State[D]())
      .render { $ =>
        <.div(
          $.state.renderer.whenDefined(
            _ { resultsPot =>
              <.div(
                resultsPot.renderPending(_ => Icon(name = "spinner", loading = true, size = Large)),
                resultsPot.render(results => $.props.valueRender(results))
              )
            }
          )
        )
      }
      .componentWillMount { $ =>
        $.props.subscribe
          .flatMap { subscription =>
            implicit val cs = $.props.cs

            $.modStateIO(_ =>
              State(
                subscription.some,
                StreamRenderer
                  .build(
                    subscription.stream
                      .map[Pot[D]](r => Ready(r))
                      .flatTap(_ => fs2.Stream.eval($.props.onNewData))
                      .cons1(Pending())
                  )
                  .some
              )
            )
          }
      }
      .componentWillUnmount($ => $.state.subscription.map(_.stop).orEmpty)
      .configure(Reusability.shouldComponentUpdate)
      .build

  val component = componentBuilder[Any]
}
