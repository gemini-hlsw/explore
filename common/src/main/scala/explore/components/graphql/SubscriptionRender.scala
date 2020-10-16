// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.graphql

import cats.effect.ConcurrentEffect
import cats.effect.IO
import cats.syntax.all._
import clue.GraphQLStreamingClient
import crystal.react._
import crystal.react.implicits._
import io.chrisdavenport.log4cats.Logger
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.collections.message.Message
import react.semanticui.elements.icon.Icon
import react.semanticui.sizes._

final case class SubscriptionRender[D, A](
  subscribe:         IO[GraphQLStreamingClient[IO, _]#Subscription[D]],
  streamModifier:    fs2.Stream[IO, D] => fs2.Stream[IO, A] = identity[fs2.Stream[IO, D]] _
)(
  val valueRender:   A => VdomNode,
  val pendingRender: Long => VdomNode = (_ => Icon(name = "spinner", loading = true, size = Large)),
  val errorRender:   Throwable => VdomNode = (t => Message(error = true)(t.getMessage)),
  val onNewData:     IO[Unit] = IO.unit
)(implicit
  val ce:            ConcurrentEffect[IO],
  val logger:        Logger[IO],
  val reuse:         Reusability[A]
) extends ReactProps(SubscriptionRender.component)
    with SubscriptionRender.Props[IO, D, A]

object SubscriptionRender {
  trait Props[F[_], D, A] {
    val subscribe: F[GraphQLStreamingClient[F, _]#Subscription[D]]
    val streamModifier: fs2.Stream[F, D] => fs2.Stream[F, A]

    val valueRender: A => VdomNode
    val pendingRender: Long => VdomNode
    val errorRender: Throwable => VdomNode
    val onNewData: F[Unit]

    implicit val ce: ConcurrentEffect[F]
    implicit val logger: Logger[F]
    implicit val reuse: Reusability[A]
  }

  final case class State[F[_], D, A](
    subscription: Option[
      GraphQLStreamingClient[F, _]#Subscription[D]
    ] = None,
    renderer:     Option[StreamRenderer.Component[A]] = None
  )

  // Reusability should be controlled by enclosing components and reuse parameter. We allow rerender every time it's requested.
  implicit def propsReuse[F[_], D, A]: Reusability[Props[F, D, A]] = Reusability.never
  implicit def stateReuse[F[_], D, A]: Reusability[State[F, D, A]] = Reusability.never

  protected def componentBuilder[F[_], D, A] =
    ScalaComponent
      .builder[Props[F, D, A]]
      .initialState(State[F, D, A]())
      .render { $ =>
        React.Fragment(
          $.state.renderer.fold[VdomNode](EmptyVdom)(
            _ {
              _.fold($.props.pendingRender, $.props.errorRender, $.props.valueRender)
            }
          )
        )
      }
      .componentDidMount { $ =>
        implicit val ce     = $.props.ce
        implicit val logger = $.props.logger
        implicit val reuse  = $.props.reuse

        $.props.subscribe
          .flatMap { subscription =>
            $.setStateIn[F](
              State(
                subscription.some,
                StreamRenderer
                  .build(
                    $.props
                      .streamModifier(subscription.stream)
                      .flatTap(_ => fs2.Stream.eval($.props.onNewData))
                  )
                  .some
              )
            )
          }
          .handleErrorWith(t => logger.error(t)("Error initializing SubscriptionRender"))
          .runInCB
      }
      .componentWillUnmount { $ =>
        implicit val ce = $.props.ce

        $.state.subscription.map(_.stop().runInCB).getOrEmpty
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

  val component = componentBuilder[IO, Any, Any]
}
