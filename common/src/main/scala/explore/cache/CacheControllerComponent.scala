// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.Resource
import cats.effect.kernel.Deferred
import cats.syntax.all.*
import crystal.Pot
import crystal.react.hooks.*
import crystal.syntax.*
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
      .useCallbackBy: props => // applyStreamUpdates: Apply updates from an update stream.
        (stream: fs2.Stream[F, Either[Throwable, S => S]]) =>
          stream
            .evalMap: modElem =>
              props.modState: oldValue =>
                modElem.fold(
                  t => Pot.Error(new RuntimeException(s"Update Error: ${t.getMessage}", t)),
                  f => oldValue.map(f)
                )
            .compile
            .drain
      .useResourceOnMountBy: (props, applyStreamUpdates) =>
        for
          _               <- Resource.eval(props.modState(_ => pending)) // Initialize on mount.
          // Start the update fiber. We want subscriptions to start before initial query.
          // This way we don't miss updates.
          // The update fiber will only update once the cache is initialized (via latch).
          // TODO: RESTART CACHE IN CASE OF INTERRUPTED SUBSCRIPTION.
          latch           <- Resource.eval(Deferred[F, Unit])
          // Next is the update fiber. It will start getting updates immediately,
          // but will wait until the cache is initialized to start applying them.
          // Will run until the component is unmounted.
          updateStreamPot <- updateStream(props).map(_.attempt)
          _               <- applyStreamUpdates(updateStreamPot).background
          // initResult is (initValue, delayedInitsStream)
          initResult      <- Resource.eval:
                               initial(props).attemptPot.map:
                                 _.adaptError: t =>
                                   new RuntimeException(s"Initialization Error: ${t.getMessage}", t)
          // Apply initial value.
          _               <- Resource.eval(props.modState(_ => initResult.map(_._1)))
          // Build and release update queue.
          _               <- Resource.eval(latch.complete(()))
          // Apply delayed inits.
          _               <- initResult.toOption
                               .map(_._2.attempt)
                               .map(applyStreamUpdates)
                               .orEmpty
                               .background
        yield ()
      .render: (_, _, _) =>
        EmptyVdom

object CacheControllerComponent:
  trait Props[S]:
    val modState: (Pot[S] => Pot[S]) => DefaultA[Unit]
