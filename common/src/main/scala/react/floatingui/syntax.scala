// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.floatingui

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.given

import scala.scalajs.js

object syntax:
  // extension (tag: VdomTag)
  //   def withTooltip(tooltip: VdomNode, placement: Placement = Placement.Top): VdomNode =
  //     Tooltip(trigger = tag, tooltip = tooltip, placement = placement)
  //   def withOptionalTooltip(
  //     tooltip:   js.UndefOr[VdomNode],
  //     placement: Placement = Placement.Top
  //   ): TagMod =
  //     tooltip.fold(tag)(tt => Tooltip(trigger = tag, tooltip = tt, placement = placement))

  extension (node: VdomNode)
    def withTooltip(tooltip: VdomNode, placement: Placement = Placement.Top): VdomNode =
      Tooltip(trigger = <.span(node), tooltip = tooltip, placement = placement)

    def withOptionalTooltip(
      tooltip:   js.UndefOr[VdomNode],
      placement: Placement = Placement.Top
    ): TagMod =
      tooltip.toOption.fold(node)(tt =>
        Tooltip(trigger = <.span(node), tooltip = tt, placement = placement)
      )
