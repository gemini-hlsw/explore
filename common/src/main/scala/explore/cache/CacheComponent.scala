// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.Async
import cats.effect.Resource
import cats.effect.kernel.Deferred
import cats.syntax.all.*
import crystal.react.hooks.*
import fs2.concurrent.SignallingRef
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.DefaultEffects.{Async => DefaultA}
import japgolly.scalajs.react.vdom.html_<^.*

trait CacheComponent[S, P <: CacheComponent.Props[S]: Reusability]:
  private type F[T] = DefaultA[T]

  protected val initial: (P, (S => S) => F[Unit]) => F[S]

  protected val updateStream: (P, (S => S) => F[Unit]) => Resource[F, fs2.Stream[F, F[S => S]]]

  val component =
    ScalaFnComponent
      .withHooks[P]
      .useEffectResultWithDepsBy(props => props)(_ =>
        props =>
          for
            latch        <- Deferred[F, SignallingRef[F, S]]
            effects      <- SignallingRef[F].of(identity[S])
            // Start the update fiber. We want subscriptions to start before initial query.
            // This way we don't miss updates.
            // The update fiber Will only update the cache once it is initialized (via latch).
            // TODO: RESTART CACHE IN CASE OF INTERRUPTED SUBSCRIPTION.
            _            <-
              updateStream(props, effects.set)
                .map(
                  _.merge(effects.discrete.map(Async[F].pure))
                )
                .evalTap(
                  _.evalTap(fmod =>
                    latch.get.flatMap(ref => fmod.flatMap(mod => ref.update(mod)))
                  ).compile.drain
                )
                .useForever
                .start
            initialValue <- initial(props, effects.set)
            cache        <- SignallingRef[F].of(initialValue)
            _            <- latch.complete(cache) // Allow stream updates to proceed.
          yield cache
      )
      .useStreamBy((props, cache) => (props, cache.isReady))((props, cache) =>
        _ =>
          cache.toOption.map(_.discrete).orEmpty.evalTap { value =>
            cats.effect.Async[F].delay(println("Emitting a value")) >>
              props.setState(value.some)
          }
      )
      // .useEffectWithDepsBy((_, _, value) => value.toOption)((props, _, _) =>
      //   value => props.setState(value)
      // )
      .render((_, _, _) => React.Fragment())

object CacheComponent:
  trait Props[S]:
    val setState: Option[S] => DefaultA[Unit]
