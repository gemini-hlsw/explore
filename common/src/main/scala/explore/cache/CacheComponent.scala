// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.Resource
import cats.effect.kernel.Deferred
import cats.syntax.all.*
import crystal.react.hooks.*
import fs2.concurrent.SignallingRef
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA
import japgolly.scalajs.react.vdom.html_<^.*

trait CacheComponent[S, P <: CacheComponent.Props[S]: Reusability]:
  private type F[T] = DefaultA[T]

  // Initial model and a stream of delayed updates.
  protected val initial: P => F[(S, fs2.Stream[F, S => S])]

  // Stream of updates to the cache. Updates are collected as soon as the
  // app starts, but they processed once all initial delayed updates complete.
  protected val updateStream: P => Resource[F, fs2.Stream[F, S => S]]

  val component =
    ScalaFnComponent
      .withHooks[P]
      .useEffectResultWithDepsBy(props => props): _ =>
        props => // TODO Could we actually useEffectStreamResource?
          for
            latch                        <- Deferred[F, SignallingRef[F, S]]
            // Start the update fiber. We want subscriptions to start before initial query.
            // This way we don't miss updates.
            // The update fiber will only update the cache once it is initialized (via latch).
            // TODO: RESTART CACHE IN CASE OF INTERRUPTED SUBSCRIPTION.
            _                            <-
              updateStream(props)
                .evalTap:
                  _.evalTap: mod =>
                    latch.get.flatMap(_.update(mod))
                  .compile.drain
                .useForever
                .start
            (initialValue, delayedInits) <- initial(props)
            cache                        <- SignallingRef[F].of(initialValue)
            _                            <-
              delayedInits
                .evalMap: mod =>
                  cache.update(mod)
                .compile
                .drain
                .flatMap: _ =>
                  latch.complete(cache) // Allow stream updates to proceed.
                .start
          yield cache
      .useStreamBy((props, cache) => (props, cache.isReady)): (props, cache) =>
        _ => cache.toOption.map(_.discrete).orEmpty.evalTap(value => props.setState(value.some))
      // .useEffectWithDepsBy((_, _, value) => value.toOption)((props, _, _) =>
      //   value => props.setState(value)
      // )
      .render((_, _, _) => React.Fragment())

object CacheComponent:
  trait Props[S]:
    val setState: Option[S] => DefaultA[Unit]
