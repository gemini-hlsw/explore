// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.floatingui.syntax

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.given
import react.floatingui.*

import scala.scalajs.js

extension (tag: VdomTag)
  def withTooltip(
    tooltip:   VdomNode,
    placement: Placement = Placement.Top
  ): VdomNode =
    Tooltip(trigger = tag, tooltip = tooltip, placement = placement)
