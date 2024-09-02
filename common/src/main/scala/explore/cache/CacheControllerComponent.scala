// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.Resource
import cats.effect.kernel.Deferred
import cats.effect.std.Queue
import cats.syntax.all.*
import crystal.react.hooks.*
import fs2.Stream
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA

trait CacheControllerComponent[S, P <: CacheControllerComponent.Props[S]]:
  private type F[T] = DefaultA[T]

  // Initial model and a stream of delayed updates.
  protected val initial: P => F[(S, fs2.Stream[F, S => S])]

  // Stream of updates to the cache. Updates are collected as soon as the
  // app starts, but they processed once all initial delayed updates complete.
  protected val updateStream: P => Resource[F, fs2.Stream[F, S => S]]

  val component =
    ScalaFnComponent
      .withHooks[P]
      .useEffectResultOnMountBy: props => // TODO Could we actually useEffectStreamResource?
        for
          _                            <- props.modState(_ => none) // Initialize on mount.
          latch                        <- Deferred[F, Queue[F, S => S]]
          // Start the update fiber. We want subscriptions to start before initial query.
          // This way we don't miss updates.
          // The update fiber will only update the cache once it is initialized (via latch).
          // TODO: RESTART CACHE IN CASE OF INTERRUPTED SUBSCRIPTION.
          _                            <-
            updateStream(props)
              .evalTap:
                _.evalTap: mod =>
                  latch.get.flatMap(_.offer(mod))
                .compile.drain
              .useForever
              .start
          (initialValue, delayedInits) <- initial(props)
          _                            <- props.modState((_: Option[S]) => initialValue.some)
          queue                        <- Queue.unbounded[F, S => S]
          _                            <- latch.complete(queue)
          _                            <-
            delayedInits
              .evalMap: mod =>
                queue.offer(mod)
              .compile
              .drain
              .start
        yield queue
      .useStreamBy((_, queue) => queue.isReady): (props, queue) =>
        _ =>
          queue.toOption
            .map(q => Stream.fromQueueUnterminated(q))
            .orEmpty
            .evalTap(f => props.modState(_.map(f)))
      .render((_, _, _) => EmptyVdom)

object CacheControllerComponent:
  trait Props[S]:
    val modState: (Option[S] => Option[S]) => DefaultA[Unit]
