// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.floatingui.syntax

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.given
import react.floatingui.*

import scala.scalajs.js

// Unfortunately, we can't differentiate between the extension methods below and use
// `withTooltip` for both because a `VdomTag` IS a `VdomNode`.

extension (tag: VdomTag)
  // This extension method can be used with html tags like `span` and `div`. These tags
  // do not need to be wrapped in another tag, so this method should be used when possible.
  def withTooltip(
    tooltip:   js.UndefOr[VdomNode],
    placement: Placement = Placement.Top
  ): VdomNode =
    tooltip.fold(tag)(tt => Tooltip(trigger = tag, tooltip = tt, placement = placement))

extension (node: VdomNode)
  // This extension method is to be used for react components, which need to be wrapped
  // in a `span` in order to be the trigger for a tooltip.
  // If possible, use `withTooltip`, above.
  def componentWithTooltip(
    tooltip:   js.UndefOr[VdomNode],
    placement: Placement = Placement.Top
  ): VdomNode =
    tooltip.toOption.fold(node)(tt =>
      Tooltip(trigger = <.span(node), tooltip = tt, placement = placement)
    )
