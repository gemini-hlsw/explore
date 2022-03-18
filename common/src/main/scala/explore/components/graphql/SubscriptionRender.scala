// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.graphql

import cats.Id
import cats.effect.Async
import cats.effect.IO
import cats.syntax.all._
import clue.GraphQLSubscription
import crystal.Pot
import crystal.react._
import crystal.react.implicits._
import crystal.react.reuse._
import japgolly.scalajs.react._
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^._
import org.typelevel.log4cats.Logger
import react.common._
import scala.reflect.ClassTag

final case class SubscriptionRender[D, A](
  subscribe:      Reuse[IO[GraphQLSubscription[IO, D]]],
  streamModifier: fs2.Stream[IO, D] ==> fs2.Stream[IO, A] =
    Reuse.always(identity[fs2.Stream[IO, D]] _)
)(
  val render:     Pot[A] ==> VdomNode,
  val onNewData:  Reuse[IO[Unit]] = Reuse.always(IO.unit)
)(implicit
  val F:          Async[IO],
  val dispatcher: Effect.Dispatch[IO],
  val logger:     Logger[IO],
  val classTag:   ClassTag[A],
  val reuse:      Reusability[A]
) extends ReactProps(SubscriptionRender.component)
    with SubscriptionRender.Props[IO, D, A]

object SubscriptionRender {
  trait Props[F[_], D, A] extends Render.Subscription.Props[F, Id, D, A]

  final case class State[F[_], D, A](
    subscription: GraphQLSubscription[F, D],
    renderer:     StreamRenderer.Component[A]
  ) extends Render.Subscription.State[F, Id, D, A]

  // Reusability should be controlled by enclosing components and reuse parameter. We allow rerender every time it's requested.
  implicit def propsReuse[F[_], D, A]: Reusability[Props[F, D, A]] =
    Reusability.by(p => (p.subscribe, p.streamModifier, p.render, p.onNewData))
  implicit def stateReuse[F[_], D, A]: Reusability[State[F, D, A]] = Reusability.never

  protected def componentBuilder[F[_], D, A] =
    ScalaComponent
      .builder[Props[F, D, A]]
      .initialState[Option[State[F, D, A]]](none)
      .render(Render.renderFn[F, Id, D, A](_))
      .componentDidMount { $ =>
        implicit val F          = $.props.F
        implicit val dispatcher = $.props.dispatcher
        implicit val logger     = $.props.logger
        implicit val reuse      = $.props.reuse

        $.props.subscribe.value
          .flatMap { subscription =>
            $.setStateIn[F](
              State(
                subscription,
                StreamRenderer
                  .build(
                    $.props
                      .streamModifier(subscription.stream)
                      .flatTap(_ => fs2.Stream.eval($.props.onNewData))
                  )
              ).some
            )
          }
          .handleErrorWith(t => logger.error(t)("Error initializing SubscriptionRender"))
          .runAsync
      }
      .componentWillUnmount(Render.Subscription.willUnmountFn[F, Id, D, A](_))
      .configure(Reusability.shouldComponentUpdate)
      .build

  val component = componentBuilder[IO, Any, Any]
}
