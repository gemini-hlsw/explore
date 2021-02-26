// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.graphql

import cats.data.NonEmptyList
import cats.effect.CancelToken
import cats.effect.ConcurrentEffect
import cats.effect.IO
import cats.effect.SyncIO
import cats.syntax.all._
import clue.GraphQLSubscription
import clue.GraphQLWebSocketClient
import clue.StreamingClientStatus
import crystal.Pot
import crystal.react.implicits._
import fs2.concurrent.Queue
import io.chrisdavenport.log4cats.Logger
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Generic.UnmountedWithRoot
import japgolly.scalajs.react.component.builder.Lifecycle.ComponentDidMount
import japgolly.scalajs.react.component.builder.Lifecycle.ComponentWillUnmount
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.vdom.html_<^._

object Render {
  trait Props[F[_], G[_], A] {
    val valueRender: G[A] => VdomNode
    val pendingRender: Long => VdomNode
    val errorRender: Throwable => VdomNode
    val onNewData: F[Unit]

    implicit val F: ConcurrentEffect[F]
    implicit val logger: Logger[F]
    implicit val reuse: Reusability[A]
  }

  type StreamRendererProps[G[_], A]     = Pot[G[A]] => VdomNode
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
        _.renderer(_.fold($.props.pendingRender, $.props.errorRender, $.props.valueRender))
      )
    )
  }

  def renderFn[F[_], G[_], D, A]: RenderApplied[F, G, D, A] = new RenderApplied[F, G, D, A]

  object Subscription {

    trait Props[F[_], G[_], D, A] extends Render.Props[F, G, A] {
      val subscribe: F[GraphQLSubscription[F, D]]
      val streamModifier: fs2.Stream[F, D] => fs2.Stream[F, A]
    }

    trait State[F[_], G[_], D, A] extends Render.State[G, A] {
      val subscription: GraphQLSubscription[F, D]
    }

    class WillUnmountApplied[F[_], G[_], D, A] {
      def apply[P <: Props[F, G, D, A], S <: State[F, G, D, A]](
        $ : ComponentWillUnmount[P, Option[S], Unit]
      ): Callback = {
        implicit val F = $.props.F

        $.state.map(_.subscription.stop().runAsyncCB).getOrEmpty
      }
    }

    def willUnmountFn[F[_], G[_], D, A]: WillUnmountApplied[F, G, D, A] =
      new WillUnmountApplied[F, G, D, A]
  }

  object LiveQuery {
    trait Props[F[_], G[_], S, D, A] extends Render.Props[F, G, A] {
      val query: F[D]
      val extract: D => A
      val changeSubscriptions: NonEmptyList[F[GraphQLSubscription[F, _]]]

      implicit val client: GraphQLWebSocketClient[F, S]
    }

    trait State[F[_], G[_], S, D, A] extends Render.State[G, A] {
      val queue: Queue[F, A]
      val subscriptions: NonEmptyList[GraphQLSubscription[F, _]]
      val cancelConnectionTracker: CancelToken[F]
    }

    class DidMountApplied[F[_], G[_], S, D, A] {
      def apply[P <: Props[F, G, S, D, A], ST <: State[F, G, S, D, A]](
        componentName: String,
        buildRenderer: (fs2.Stream[F, A], P) => StreamRendererComponent[G, A],
        buildState:    (Queue[F, A], NonEmptyList[GraphQLSubscription[F, _]], CancelToken[F],
          StreamRendererComponent[G, A]) => ST
      )(
        $             : ComponentDidMount[P, Option[ST], Unit]
      ): Callback = {
        implicit val F      = $.props.F
        implicit val logger = $.props.logger

        def queryAndEnqueue(queue: Queue[F, A]): F[Unit] =
          for {
            result <- $.props.query.map($.props.extract)
            _      <- queue.enqueue1(result)
            _      <- $.props.onNewData
          } yield ()

        // Once run, this effect will end when all subscriptions end.
        def trackChanges(
          subscriptions: NonEmptyList[GraphQLSubscription[F, _]],
          queue:         Queue[F, A]
        ): F[Unit] =
          subscriptions
            .map(_.stream)
            .reduceLeft(_ merge _)
            .evalTap(_ => queryAndEnqueue(queue))
            .compile
            .drain

        // Once run, this effect has to be cancelled manually.
        def trackConnection(queue: Queue[F, A]): F[Unit] =
          $.props.client.statusStream.tail // Skip current status. We only want future updates here.
            .filter(_ === StreamingClientStatus.Connected)
            .evalTap(_ => queryAndEnqueue(queue))
            .compile
            .drain

        def deferRun[B, C](unhandledRun: (Either[Throwable, C] => IO[Unit]) => SyncIO[B]): F[B] =
          F.liftIO(unhandledRun {
            case Left(t) => F.toIO(logger.error(t)(s"Error in deferRun of $componentName"))
            case _       => IO.unit
          }.toIO)

        val init =
          for {
            queue                   <- Queue.unbounded[F, A]
            subscriptions           <- $.props.changeSubscriptions.sequence
            cancelConnectionTracker <- deferRun(F.runCancelable(trackConnection(queue)))
            renderer                 = buildRenderer(queue.dequeue, $.props)
            _                       <-
              $.setStateIn[F](
                buildState(queue, subscriptions, cancelConnectionTracker, renderer).some
              )
            _                       <- queryAndEnqueue(queue)
            _                       <- deferRun(
                                         F.runAsync(
                                           trackChanges(subscriptions, queue)
                                             .handleErrorWith(t => logger.error(t)(s"Error updating $componentName"))
                                         )
                                       )
          } yield ()

        init
          .handleErrorWith(t => logger.error(t)(s"Error initializing $componentName"))
          .runAsyncCB
      }
    }

    def didMountFn[F[_], G[_], S, D, A]: DidMountApplied[F, G, S, D, A] =
      new DidMountApplied[F, G, S, D, A]

    class WillUnmountApplied[F[_], G[_], S, D, A] {
      def apply[P <: Props[F, G, S, D, A], ST <: State[F, G, S, D, A]](
        $ : ComponentWillUnmount[P, Option[ST], Unit]
      ): Callback = {
        implicit val F = $.props.F

        $.state
          .map(state =>
            (state.cancelConnectionTracker >>
              state.subscriptions.map(_.stop()).sequence.void).runAsyncCB
          )
          .getOrEmpty
      }
    }

    def willUnmountFn[F[_], G[_], S, D, A]: WillUnmountApplied[F, G, S, D, A] =
      new WillUnmountApplied[F, G, S, D, A]
  }
}
