// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.graphql

import cats.Monoid
import cats.effect.ConcurrentEffect
import cats.effect.IO
import cats.implicits._
import clue.GraphQLStreamingClient
import crystal.react.StreamRenderer
import crystal.react.implicits._
import diode.data._
import diode.react.ReactPot._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.elements.icon.Icon
import react.semanticui.sizes._

final case class SubscriptionRender[D, A](
  subscribe:       IO[GraphQLStreamingClient[IO]#Subscription[D]],
  streamModifier:  fs2.Stream[IO, D] => fs2.Stream[IO, A] = identity[fs2.Stream[IO, D]] _
)(
  val valueRender: A => VdomNode,
  val onNewData:   IO[Unit] = IO.unit
)(implicit
  val ce:          ConcurrentEffect[IO]
) extends SubscriptionRender.Props[IO, D, A]
    with ReactProps       {
  override def render: VdomElement =
    SubscriptionRender.component(this.asInstanceOf[SubscriptionRender.Props[IO, Any, Any]])
}

object SubscriptionRender {
  trait Props[F[_], D, A] {
    val subscribe: F[GraphQLStreamingClient[F]#Subscription[D]]
    val streamModifier: fs2.Stream[F, D] => fs2.Stream[F, A]
    val valueRender: A => VdomNode
    val onNewData: F[Unit]
    implicit val ce: ConcurrentEffect[F]
  }

  final case class State[F[_], D, A](
    subscription: Option[
      GraphQLStreamingClient[F]#Subscription[D]
    ] = None,
    renderer:     Option[StreamRenderer.Component[Pot[A]]] = None
  )

  implicit def propsReuse[F[_], D, A]: Reusability[Props[F, D, A]] = Reusability.always
  implicit def stateReuse[F[_], D, A]: Reusability[State[F, D, A]] = Reusability.never

  protected def componentBuilder[F[_], D, A](implicit monoid: Monoid[F[Unit]]) =
    ScalaComponent
      .builder[Props[F, D, A]]("SubscriptionRender")
      .initialState(State[F, D, A]())
      .render { $ =>
        React.Fragment(
          $.state.renderer.fold[VdomNode](EmptyVdom)(
            _ { resultsPot =>
              React.Fragment(
                resultsPot.renderPending(_ => Icon(name = "spinner", loading = true, size = Large)),
                resultsPot.render(results => $.props.valueRender(results))
              )
            }
          )
        )
      }
      .componentWillMount { $ =>
        implicit val ce = $.props.ce

        $.props.subscribe.flatMap { subscription =>
          $.setStateIn[F](
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
        }.runInCB
      }
      .componentWillUnmount { $ =>
        implicit val ce = $.props.ce

        $.state.subscription.map(_.stop).orEmpty.runInCB
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

  val component = componentBuilder[IO, Any, Any]
}
