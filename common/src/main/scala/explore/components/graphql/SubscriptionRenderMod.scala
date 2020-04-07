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
import crystal.react.implicits._
import diode.data._
import diode.react.ReactPot._
import cats.effect.ContextShift
import crystal.react.StreamRendererMod
import scala.concurrent.duration._
import scala.language.postfixOps
import cats.effect.Timer

final case class SubscriptionRenderMod[D, A](
  subscribe:      IO[GraphQLStreamingClient[IO]#Subscription[D]],
  streamModifier: fs2.Stream[IO, D] => fs2.Stream[IO, A] = identity[fs2.Stream[IO, D]] _
)(
  val valueRender: View[IO, A] => VdomNode,
  val onNewData:   IO[Unit] = IO.unit
)(
  implicit val cs: ContextShift[IO],
  val timer:       Timer[IO]
) extends ReactProps {
  override def render: VdomElement =
    SubscriptionRenderMod.component(this.asInstanceOf[SubscriptionRenderMod[Any, Any]])
}

object SubscriptionRenderMod {
  protected type Props[D, A] = SubscriptionRenderMod[D, A]

  protected final case class State[D, A](
    subscription: Option[
      GraphQLStreamingClient[IO]#Subscription[D]
    ] = None,
    renderer: Option[StreamRendererMod[IO, Pot[A]]] = None
  )

  implicit protected def propsReuse[D, A]: Reusability[Props[D, A]] = Reusability.always
  implicit protected def stateReuse[D, A]: Reusability[State[D, A]] = Reusability.never

  protected def componentBuilder[D, A] =
    ScalaComponent
      .builder[Props[D, A]]("SubscriptionRenderMod")
      .initialState(State[D, A]())
      .render { $ =>
        <.div(
          $.state.renderer.whenDefined(
            _ { view =>
              <.div(
                view.value.renderPending(_ => Icon(name = "spinner", loading = true, size = Large)),
                view.value.render(_ =>
                  $.props.valueRender(
                    view.zoom(_.get)(f => _.map(f))
                  )
                )
              )
            }
          )
        )
      }
      .componentWillMount { $ =>
        $.props.subscribe.flatMap { subscription =>
          implicit val cs    = $.props.cs
          implicit val timer = $.props.timer

          $.setStateIn[IO](
            State(
              subscription.some,
              StreamRendererMod
                .build(
                  $.props
                    .streamModifier(subscription.stream)
                    .map[Pot[A]](a => Ready(a))
                    .flatTap(_ => fs2.Stream.eval($.props.onNewData))
                    .cons1(Pending()),
                  holdAfterMod = (2 seconds).some
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
