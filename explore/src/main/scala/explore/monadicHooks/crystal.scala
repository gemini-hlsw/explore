// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.monadicHooks

import crystal.react.hooks.UseStateView
import crystal.react.View

// Crystal
inline def useStateView[A]: A => HookResult[View[A]] =
  UseStateView.hook.toHookResult
