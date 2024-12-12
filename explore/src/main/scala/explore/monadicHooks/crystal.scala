// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.monadicHooks

import crystal.react.View
import crystal.react.hooks.*
import japgolly.scalajs.react.Reusable
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA

/**
 * Provides a context in which to run a single effect at a time. When a new effect is submitted, the
 * previous one is canceled. Also cancels the effect on unmount.
 *
 * A submitted effect can be explicitly canceled too.
 */
inline def useSingleEffect: HookResult[Reusable[UseSingleEffect[DefaultA]]] =
  UseSingleEffect.hook.toHookResult

/** Creates component state as a View */
inline def useStateView[A]: A => HookResult[View[A]] =
  UseStateView.hook.toHookResult

// Custom hook example
import cats.syntax.all.*
import japgolly.scalajs.react.Callback

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
