// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.monadicHooks

import japgolly.scalajs.react.hooks.Hooks.*
import japgolly.scalajs.react.feature.Context
import crystal.react.hooks.UseStateView
import crystal.react.View
import cats.effect.IO

def useContext[A](ctx: Context[A]): HookResult[A] = HookResult:
  UseContext.unsafeCreate(ctx)

def useState[A](initial: A): HookResult[UseState[A]] = HookResult:
  UseState.unsafeCreate(initial)

def useStateView[A](initial: A): HookResult[View[A]] = HookResult:
  UseStateView.hook.unsafeInit(initial)

def useEffectOnMount(effect: IO[Unit]): HookResult[Unit] = HookResult:
  UseEffect.unsafeCreateOnMount(effect)
