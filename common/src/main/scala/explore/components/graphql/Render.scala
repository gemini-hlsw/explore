// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.graphql

import cats.effect._
import cats.effect.std.Queue
import cats.effect.syntax.all._
import cats.syntax.all._
import clue.GraphQLSubscription
import clue.PersistentClientStatus
import clue.WebSocketClient
import crystal.Pot
import crystal.react.implicits._
import crystal.react.reuse._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Generic.UnmountedWithRoot
import japgolly.scalajs.react.component.builder.Lifecycle.ComponentDidMount
import japgolly.scalajs.react.component.builder.Lifecycle.ComponentWillUnmount
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^._
import org.typelevel.log4cats.Logger
import scala.reflect.ClassTag

object Render {
  trait Props[F[_], G[_], A] {
    val render: Pot[G[A]] ==> VdomNode
    val onNewData: Reuse[F[Unit]]

    implicit val F: Async[F]
    implicit val dispatcher: Effect.Dispatch[F]
    implicit val logger: Logger[F]
    implicit val classTag: ClassTag[A]
    implicit val reuse: Reusability[A]
  }

  type StreamRendererProps[G[_], A]     = Pot[G[A]] ==> VdomNode
  type StreamRendererComponent[G[_], A] =
    CtorType.Props[StreamRendererProps[G, A], UnmountedWithRoot[
      StreamRendererProps[G, A],
      _,
      _,
      _
    ]]

  trait State[G[_], A] {
    val renderer: StreamRendererComponent[G, A]
  }

  class RenderApplied[F[_], G[_], D, A] {
    def apply[P <: Props[F, G, A], S <: State[G, A]](
      $ : RenderScope[P, Option[S], Unit]
    ): VdomNode = React.Fragment(
      $.state.fold[VdomNode](EmptyVdom)(
        _.renderer($.props.render)
      )
    )
  }

  def renderFn[F[_], G[_], D, A]: RenderApplied[F, G, D, A] = new RenderApplied[F, G, D, A]

  object Subscription {

    trait Props[F[_], G[_], D, A] extends Render.Props[F, G, A] {
      val subscribe: Reuse[F[GraphQLSubscription[F, D]]]
      val streamModifier: fs2.Stream[F, D] ==> fs2.Stream[F, A]
    }

    trait State[F[_], G[_], D, A] extends Render.State[G, A] {
      val subscription: GraphQLSubscription[F, D]
    }

    class WillUnmountApplied[F[_], G[_], D, A] {
      def apply[P <: Props[F, G, D, A], S <: State[F, G, D, A]](
        $ : ComponentWillUnmount[P, Option[S], Unit]
      ): Callback = {
        implicit val F          = $.props.F
        implicit val dispatcher = $.props.dispatcher
        implicit val logger     = $.props.logger

        $.state.map(_.subscription.stop().runAsync).getOrEmpty
      }
    }

    def willUnmountFn[F[_], G[_], D, A]: WillUnmountApplied[F, G, D, A] =
      new WillUnmountApplied[F, G, D, A]
  }

  object LiveQuery {
    trait Props[F[_], G[_], S, D, A] extends Render.Props[F, G, A] {
      val query: Reuse[F[D]]
      val extract: D ==> A
      val changeSubscriptions: Reuse[List[F[GraphQLSubscription[F, _]]]]

      implicit val client: WebSocketClient[F, S]
    }

    trait State[F[_], G[_], S, D, A] extends Render.State[G, A] {
      val queue: Queue[F, A]
      val subscriptions: List[GraphQLSubscription[F, _]]
      val cancelConnectionTracker: F[Unit]
    }

    class DidMountApplied[F[_], G[_], S, D, A] {
      def apply[P <: Props[F, G, S, D, A], ST <: State[F, G, S, D, A]](
        componentName: String,
        buildRenderer: (fs2.Stream[F, A], P) => StreamRendererComponent[G, A],
        buildState:    (
          Queue[F, A],
          List[GraphQLSubscription[F, _]],
          F[Unit],
          StreamRendererComponent[G, A]
        ) => ST
      )($            : ComponentDidMount[P, Option[ST], Unit]): Callback = {
        // import $.props._
        implicit val F          = $.props.F
        implicit val dispatcher = $.props.dispatcher
        implicit val logger     = $.props.logger

        def queryAndEnqueue(queue: Queue[F, A]): F[Unit] =
          for {
            result <- $.props.query.value.map($.props.extract)
            _      <- queue.offer(result)
            _      <- $.props.onNewData.value
          } yield ()

        // Once run, this effect will end when all subscriptions end.
        def trackChanges(
          subscriptions: List[GraphQLSubscription[F, _]],
          queue:         Queue[F, A]
        ): F[Unit] =
          subscriptions match {
            case Nil => F.unit
            case _   =>
              subscriptions
                .map(_.stream)
                .reduceLeft(_ merge _)
                .evalTap(_ => queryAndEnqueue(queue))
                .compile
                .drain
          }

        // Once run, this effect has to be cancelled manually.
        def trackConnection(queue: Queue[F, A]): F[Unit] =
          $.props.client.statusStream.tail // Skip current status. We only want future updates here.
            .filter(_ === PersistentClientStatus.Connected)
            .evalTap(_ => queryAndEnqueue(queue))
            .compile
            .drain

        def runCancelableWithLog(f: F[Unit]): F[F[Unit]] =
          f.onError(t => logger.error(t)(s"Error in runCancelableWithLog of $componentName"))
            .start
            .map(_.cancel)

        val init =
          for {
            queue                   <- Queue.unbounded[F, A]
            subscriptions           <- $.props.changeSubscriptions.value.sequence
            cancelConnectionTracker <- runCancelableWithLog(trackConnection(queue))
            renderer                 = buildRenderer(fs2.Stream.fromQueueUnterminated(queue), $.props)
            _                       <-
              $.setStateIn[F](
                buildState(queue, subscriptions, cancelConnectionTracker, renderer).some
              )
            _                       <- queryAndEnqueue(queue)
            _                       <- runCancelableWithLog(trackChanges(subscriptions, queue))
          } yield ()

        init
          .handleErrorWith(t => logger.error(t)(s"Error initializing $componentName"))
          .runAsync
      }
    }

    def didMountFn[F[_], G[_], S, D, A]: DidMountApplied[F, G, S, D, A] =
      new DidMountApplied[F, G, S, D, A]

    class WillUnmountApplied[F[_], G[_], S, D, A] {
      def apply[P <: Props[F, G, S, D, A], ST <: State[F, G, S, D, A]](
        $ : ComponentWillUnmount[P, Option[ST], Unit]
      ): Callback = {
        implicit val F          = $.props.F
        implicit val dispatcher = $.props.dispatcher
        implicit val logger     = $.props.logger

        $.state
          .map(state =>
            (state.cancelConnectionTracker >>
              state.subscriptions.map(_.stop().handleError(_ => ())).sequence.void).runAsync
          )
          .getOrEmpty
      }
    }

    def willUnmountFn[F[_], G[_], S, D, A]: WillUnmountApplied[F, G, S, D, A] =
      new WillUnmountApplied[F, G, S, D, A]
  }
}
