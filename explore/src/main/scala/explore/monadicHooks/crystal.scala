// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.monadicHooks

import crystal.react.hooks.*
import crystal.react.View
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
