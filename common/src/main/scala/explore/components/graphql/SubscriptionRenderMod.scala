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
import crystal.react.StreamRendererMod
import scala.concurrent.duration._
import scala.language.postfixOps
import cats.effect.Timer
import cats.effect.ConcurrentEffect
import cats.Monoid

final case class SubscriptionRenderMod[D, A](
  subscribe:       IO[GraphQLStreamingClient[IO]#Subscription[D]],
  streamModifier:  fs2.Stream[IO, D] => fs2.Stream[IO, A] = identity[fs2.Stream[IO, D]] _
)(
  val valueRender: View[IO, A] => VdomNode,
  val onNewData:   IO[Unit] = IO.unit
)(implicit
  val ce:          ConcurrentEffect[IO],
  val timer:       Timer[IO]
) extends SubscriptionRenderMod.Props[IO, D, A]
    with ReactProps          {
  override def render: VdomElement =
    SubscriptionRenderMod.component(this.asInstanceOf[SubscriptionRenderMod.Props[IO, Any, Any]])
}

object SubscriptionRenderMod {
  trait Props[F[_], D, A] {
    val subscribe: F[GraphQLStreamingClient[F]#Subscription[D]]
    val streamModifier: fs2.Stream[F, D] => fs2.Stream[F, A]
    val valueRender: View[F, A] => VdomNode
    val onNewData: F[Unit]
    implicit val ce: ConcurrentEffect[F]
    implicit val timer: Timer[F]
  }

  protected final case class State[F[_], D, A](
    subscription: Option[
      GraphQLStreamingClient[F]#Subscription[D]
    ] = None,
    renderer:     Option[StreamRendererMod[F, Pot[A]]] = None
  )

  implicit protected def propsReuse[F[_], D, A]: Reusability[Props[F, D, A]] = Reusability.always
  implicit protected def stateReuse[F[_], D, A]: Reusability[State[F, D, A]] = Reusability.never

  protected def componentBuilder[F[_], D, A](implicit monoid: Monoid[F[Unit]]) =
    ScalaComponent
      .builder[Props[F, D, A]]("SubscriptionRenderMod")
      .initialState(State[F, D, A]())
      .render { $ =>
        <.div(
          $.state.renderer.whenDefined(
            _ { view =>
              <.div(
                view.get.renderPending(_ => Icon(name = "spinner", loading = true, size = Large)),
                view.get.render(_ =>
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
        implicit val ce    = $.props.ce
        implicit val timer = $.props.timer

        $.props.subscribe.flatMap { subscription =>
          $.setStateIn[F](
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
