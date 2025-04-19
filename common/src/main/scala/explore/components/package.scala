// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.TooltipOptions

object ToolbarTooltipOptions:
  val Default =
    TooltipOptions(
      showOnDisabled = true,
      position = Tooltip.Position.Top,
      showDelay = 200
    )
