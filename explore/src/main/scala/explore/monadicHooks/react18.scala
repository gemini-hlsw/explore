// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.monadicHooks

import japgolly.scalajs.react.hooks.Hooks.*

/**
 * `useId` is a React Hook for generating unique IDs that can be passed to accessibility attributes.
 *
 * @see
 *   https://react.dev/reference/react/useId
 */
inline def useId: HookResult[String] =
  UseId().toHookResult

/**
 * Allows components to avoid undesirable loading states by waiting for content to load before
 * transitioning to the next screen. It also allows components to defer slower, data fetching
 * updates until subsequent renders so that more crucial updates can be rendered immediately.
 *
 * **If some state update causes a component to suspend, that state update should be wrapped in a
 * transition.**
 *
 * @see
 *   {@link https://react.dev/reference/react/useTransition}
 */
inline def useTransition: HookResult[UseTransition] =
  UseTransition().toHookResult
