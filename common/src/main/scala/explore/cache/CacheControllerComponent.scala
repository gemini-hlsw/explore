// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.Resource
import cats.effect.kernel.Deferred
import cats.effect.std.Queue
import cats.syntax.all.*
import crystal.Pot
import crystal.react.hooks.*
import crystal.syntax.*
import fs2.Stream
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.effect.*

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
      // TODO We should useEffectStreamResource, so that the update stream stops when the component is unmounted (changing program).
      .useEffectResultOnMountBy: props =>
        for
          _          <- props.modState(_ => pending) // Initialize on mount.
          latch      <- Deferred[F, Queue[F, S => S]]
          // Start the update fiber. We want subscriptions to start before initial query.
          // This way we don't miss updates.
          // The update fiber will only update the cache once it is initialized (via latch).
          // TODO: RESTART CACHE IN CASE OF INTERRUPTED SUBSCRIPTION.
          _          <-
            updateStream(props)
              .evalTap:
                _.evalTap: mod =>
                  latch.get.flatMap(_.offer(mod))
                .compile.drain
              .useForever
              .start
          initResult <- initial(props).attemptPot.map:
                          _.adaptError: t =>
                            new RuntimeException(s"InitializationError: ${t.getMessage}", t)
          _          <- props.modState(_ => initResult.map(_._1))
          queue      <- Queue.unbounded[F, S => S]
          _          <- latch.complete(queue)
          _          <-
            initResult.toOption
              .map(_._2)
              .map: delayedInits =>
                delayedInits
                  .evalMap: mod =>
                    queue.offer(mod)
                  .compile
                  .drain
                  .start // TODO Change to background when we switch to Resource
                  .void
              .orEmpty
        yield queue
      .useStreamBy((_, queue) => queue.isReady): (props, queue) =>
        _ =>
          queue.toOption
            .map(q => Stream.fromQueueUnterminated(q))
            .orEmpty
            .evalTap(f => props.modState(_.map(f)))
      .render: (_, initialPot, updateStreamPotOption) =>
        EmptyVdom

object CacheControllerComponent:
  trait Props[S]:
    val modState: (Pot[S] => Pot[S]) => DefaultA[Unit]
