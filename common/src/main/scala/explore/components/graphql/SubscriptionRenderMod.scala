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
import crystal.react.io.implicits._
import diode.data._
import diode.react.ReactPot._
import cats.effect.ContextShift
import crystal.react.StreamRendererMod

final case class SubscriptionRenderMod[D, A](
  subscribe:      IO[GraphQLStreamingClient[IO]#Subscription[D]],
  streamModifier: fs2.Stream[IO, D] => fs2.Stream[IO, A] = identity[fs2.Stream[IO, D]] _
)(
  val valueRender: (A, StreamRendererMod.ModState[A]) => VdomNode,
  val onNewData:   IO[Unit] = IO.unit
)(
  implicit val cs: ContextShift[IO]
) extends ReactProps {
  override def render: VdomElement =
    SubscriptionRenderMod.component(this.asInstanceOf[SubscriptionRenderMod[Any, Any]])
}

object SubscriptionRenderMod {
  type Props[D, A] = SubscriptionRenderMod[D, A]

  final case class State[D, A](
    subscription: Option[
      GraphQLStreamingClient[IO]#Subscription[D]
    ] = None,
    renderer: Option[StreamRendererMod[Pot[A]]] = None
  )

  implicit def propsReuse[D, A]: Reusability[Props[D, A]] = Reusability.always
  implicit def stateReuse[D, A]: Reusability[State[D, A]] = Reusability.never

  protected def componentBuilder[D, A] =
    ScalaComponent
      .builder[Props[D, A]]("SubscriptionRenderMod")
      .initialState(State[D, A]())
      .render { $ =>
        <.div(
          $.state.renderer.whenDefined(
            _ { (resultsPot, modState) =>
              <.div(
                resultsPot.renderPending(_ => Icon(name = "spinner", loading = true, size = Large)),
                resultsPot.render(results => $.props.valueRender(results, f => modState(_.map(f))))
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
                StreamRendererMod
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
          }
      }
      .componentWillUnmount($ => $.state.subscription.map(_.stop).orEmpty)
      .configure(Reusability.shouldComponentUpdate)
      .build

  val component = componentBuilder[Any, Any]
}
