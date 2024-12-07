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
      .useResourceOnMountBy: props =>
        for
          _          <- Resource.eval(props.modState(_ => pending)) // Initialize on mount.
          // Start the update fiber. We want subscriptions to start before initial query.
          // This way we don't miss updates.
          // The update fiber will only update the cache once it is initialized (via latch).
          // TODO: RESTART CACHE IN CASE OF INTERRUPTED SUBSCRIPTION.
          latch      <- Resource.eval(Deferred[F, Queue[F, Either[Throwable, S => S]]])
          // Next is the update fiber. It will start getting updates immediately,
          // but will wait until the cache is initialized to start applying them.
          // Will run until the component is unmounted.
          _          <- updateStream(props)
                          .map(_.attempt)
                          .evalTap:
                            _.evalTap: mod =>
                              latch.get.flatMap(_.offer(mod))
                            .compile.drain
                          .useForever
                          .background
          // initResult is (initValue, delayedInitsStream)
          initResult <- Resource.eval:
                          initial(props).attemptPot.map:
                            _.adaptError: t =>
                              new RuntimeException(s"Initialization Error: ${t.getMessage}", t)
          // Apply initial value.
          _          <- Resource.eval(props.modState(_ => initResult.map(_._1)))
          // Build and release update queue.
          queue      <- Resource.eval(Queue.unbounded[F, Either[Throwable, S => S]])
          _          <- Resource.eval(latch.complete(queue))
          // Apply delayed inits.
          _          <- initResult.toOption
                          .map(_._2)
                          .map: delayedInits =>
                            delayedInits.attempt
                              .evalMap: mod =>
                                queue.offer(mod)
                              .compile
                              .drain
                          .orEmpty
                          .background
        yield queue
      .useEffectStreamWhenDepsReadyBy((_, queue) => queue): (props, _) =>
        queue =>
          Stream
            .fromQueueUnterminated(queue)
            .evalMap: elem =>
              props.modState: oldValue =>
                elem match
                  case Left(t)  => Pot.Error(t)
                  case Right(f) => oldValue.map(f)
      .render: (_, _) =>
        EmptyVdom

object CacheControllerComponent:
  trait Props[S]:
    val modState: (Pot[S] => Pot[S]) => DefaultA[Unit]
