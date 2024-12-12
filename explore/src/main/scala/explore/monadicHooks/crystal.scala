// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.monadicHooks

import crystal.react.View
import crystal.react.ReuseView
import crystal.react.hooks.*
import japgolly.scalajs.react.Reusable
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA
import japgolly.scalajs.react.util.DefaultEffects.Sync as DefaultS

/**
 * Provides a context in which to run a single effect at a time. When a new effect is submitted, the
 * previous one is canceled. Also cancels the effect on unmount.
 *
 * A submitted effect can be explicitly canceled too.
 */
inline def useSingleEffect: HookResult[Reusable[UseSingleEffect[DefaultA]]] =
  UseSingleEffect.hook.toHookResult

/** Creates component state that is reused while it's not updated. */
inline def useSerialState[A](initialValue: => A): HookResult[UseSerialState[A]] =
  UseSerialState.hook(initialValue).toHookResult

/**
 * Given a state, allows registering callbacks which are triggered when the state changes.
 */
inline def useStateCallback[A](
  state: => Hooks.UseState[A]
): HookResult[Reusable[(A => DefaultS[Unit]) => DefaultS[Unit]]] =
  UseStateCallback.hook(state).toHookResult

/** Creates component state as a View */
inline def useStateView[A]: A => HookResult[View[A]] =
  UseStateView.hook.toHookResult

/** Creates component state as a View */
inline def useStateViewWithReuse[A: ClassTag: Reusability]: A => HookResult[ReuseView[A]] =
  UseStateViewWithReuse.hook.toHookResult

/** Creates component state as a View that is reused while it's not updated. */
inline def useSerialStateView[A]: A => HookResult[ReuseView[A]] =
  UseSerialStateView.hook.toHookResult

/**
 * Run async effect and cancel previously running instances, thus avoiding race conditions. Allows
 * returning a cleanup effect.
 */
// inline def useAsyncEffect[G](effect: G)(using EffectWithCleanup[G, DefaultA]): HookResult[Unit] =
// HookResult:
//   UseAsyncEffect.hook(WithDeps(NeverReuse, effect)) // ... urgh protected things...
// useAsyncEffectWithDepsBy(_ => NeverReuse)(ctx => (_: Reuse[Unit]) => effect(ctx))

// Custom hook example
import cats.syntax.all.*
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.hooks.Hooks
import scala.reflect.ClassTag
import japgolly.scalajs.react.Reusability

// TODO REMOVE, this is just PoC
/**
 * Custom hook that keeps a counter and provides a function that adds to it and reports the new
 * value to the console.
 */
inline def useAddReporter(initial: Int): HookResult[Reusable[Int => Callback]] =
  for
    state <- useState(initial)
    cb    <- useCallback: (i: Int) =>
               state.modState(_ + i) >> Callback.log(s"New value ${state.value + i}")
  yield cb
