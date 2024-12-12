// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.monadicHooks

import japgolly.scalajs.react.hooks.Hooks.*
import japgolly.scalajs.react.feature.Context
import crystal.react.hooks.UseStateView
import crystal.react.View
import cats.effect.IO
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.Reusable

def useContext[A](ctx: Context[A]): HookResult[A] =
  HookResult:
    UseContext.unsafeCreate(ctx)

def useState[A](initial: A): HookResult[UseState[A]] =
  HookResult:
    UseState.unsafeCreate(initial)

def useStateView[A]: A => HookResult[View[A]] =
  UseStateView.hook.lift

def useEffectOnMount(effect: IO[Unit]): HookResult[Unit] =
  HookResult:
    UseEffect.unsafeCreateOnMount(effect)

def useEffectWithDeps[D: Reusability](deps: => D)(effect: D => IO[Unit]): HookResult[Unit] =
  HookResult:
    ReusableEffect.useEffect(deps)(effect).unsafeInit(())

def useMemo[D: Reusability, A](deps: => D)(create: D => A): HookResult[Reusable[A]] =
  UseMemo(deps)(create).lift
