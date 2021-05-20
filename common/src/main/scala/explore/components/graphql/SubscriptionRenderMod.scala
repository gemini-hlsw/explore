// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.graphql

import cats.effect.Async
import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.syntax.all._
import clue.GraphQLSubscription
import crystal.Pot
import crystal.ViewF
import crystal.react._
import crystal.react.implicits._
import explore.View
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.typelevel.log4cats.Logger
import react.common._
import react.semanticui.collections.message.Message
import react.semanticui.elements.loader.Loader

import scala.concurrent.duration._
import scala.language.postfixOps

final case class SubscriptionRenderMod[D, A](
  subscribe:         IO[GraphQLSubscription[IO, D]],
  streamModifier:    fs2.Stream[IO, D] => fs2.Stream[IO, A] = identity[fs2.Stream[IO, D]] _
)(
  val valueRender:   View[A] ~=> VdomNode,
  val pendingRender: Long ~=> VdomNode = Reusable.always(_ => Loader(active = true)),
  val errorRender:   Throwable ~=> VdomNode =
    Reusable.always(t => Message(error = true)(t.getMessage)),
  val onNewData:     IO[Unit] = IO.unit
)(implicit
  val F:             Async[IO],
  val dispatcher:    Dispatcher[IO],
  val logger:        Logger[IO],
  val reuse:         Reusability[A]
) extends ReactProps(SubscriptionRenderMod.component)
    with SubscriptionRenderMod.Props[IO, D, A]

object SubscriptionRenderMod {
  trait Props[F[_], D, A] extends Render.Subscription.Props[F, ViewF[F, *], D, A] {}

  protected final case class State[F[_], D, A](
    subscription: GraphQLSubscription[F, D],
    renderer:     StreamRendererMod.Component[F, A]
  ) extends Render.Subscription.State[F, ViewF[F, *], D, A]

  // Reusability should be controlled by enclosing components and reuse parameter. We allow rerender every time it's requested.
  implicit protected def propsReuse[F[_], D, A]: Reusability[Props[F, D, A]] =
    Reusability.never
  implicit protected def stateReuse[F[_], D, A]: Reusability[State[F, D, A]] = Reusability.never

  implicit protected def renderReuse[F[_], A]: Reusability[Pot[ViewF[F, A]] => VdomNode] =
    Reusability.never

  protected def componentBuilder[F[_], D, A] =
    ScalaComponent
      .builder[Props[F, D, A]]
      .initialState[Option[State[F, D, A]]](none)
      .render(Render.renderFn[F, ViewF[F, *], D, A](_))
      .componentDidMount { $ =>
        implicit val F          = $.props.F
        implicit val dispatcher = $.props.dispatcher
        implicit val logger     = $.props.logger
        implicit val reuse      = $.props.reuse

        $.props.subscribe
          .flatMap { subscription =>
            $.setStateIn[F](
              State(
                subscription,
                StreamRendererMod
                  .build(
                    $.props
                      .streamModifier(subscription.stream)
                      .flatTap(_ => fs2.Stream.eval($.props.onNewData)),
                    holdAfterMod = (2 seconds).some
                  )
              ).some
            )
          }
          .handleErrorWith(t => logger.error(t)("Error initializing SubscriptionRenderMod"))
          .runAsyncCB
      }
      .componentWillUnmount(Render.Subscription.willUnmountFn[F, ViewF[F, *], D, A](_))
      .configure(Reusability.shouldComponentUpdate)
      .build

  val component = componentBuilder[IO, Any, Any]
}
