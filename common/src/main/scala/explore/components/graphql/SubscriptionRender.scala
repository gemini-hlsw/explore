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
import crystal.react.implicits._
import diode.data._
import diode.react.ReactPot._
import cats.effect.ContextShift

final case class SubscriptionRender[D, A](
  subscribe:      IO[GraphQLStreamingClient[IO]#Subscription[D]],
  streamModifier: fs2.Stream[IO, D] => fs2.Stream[IO, A] = identity[fs2.Stream[IO, D]] _
)(
  val valueRender: A => VdomNode,
  val onNewData:   IO[Unit] = IO.unit
)(
  implicit val cs: ContextShift[IO]
) extends ReactProps {
  override def render: VdomElement =
    SubscriptionRender.component(this.asInstanceOf[SubscriptionRender[Any, Any]])
}

object SubscriptionRender {
  type Props[D, A] = SubscriptionRender[D, A]

  final case class State[D, A](
    subscription: Option[
      GraphQLStreamingClient[IO]#Subscription[D]
    ] = None,
    renderer: Option[StreamRenderer[Pot[A]]] = None
  )

  implicit def propsReuse[D, A]: Reusability[Props[D, A]] = Reusability.always
  implicit def stateReuse[D, A]: Reusability[State[D, A]] = Reusability.never

  protected def componentBuilder[D, A] =
    ScalaComponent
      .builder[Props[D, A]]("SubscriptionRender")
      .initialState(State[D, A]())
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
        $.props.subscribe.flatMap { subscription =>
          implicit val cs = $.props.cs

          $.setStateIn[IO](
            State(
              subscription.some,
              StreamRenderer
                .build(
                  $.props
                    .streamModifier(subscription.stream)
                    .map[Pot[A]](a => Ready(a))
                    .flatTap(_ => fs2.Stream.eval($.props.onNewData))
                    .cons1(Pending())
                )
                .some
            )
          )
        }.toCB
      }
      .componentWillUnmount($ => $.state.subscription.map(_.stop).orEmpty.toCB)
      .configure(Reusability.shouldComponentUpdate)
      .build

  val component = componentBuilder[Any, Any]
}
