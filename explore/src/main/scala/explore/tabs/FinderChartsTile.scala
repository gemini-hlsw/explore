// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.findercharts.FinderCharts
import lucuma.ui.syntax.all.given

object FinderChartsTile:

  def finderChartsTile =
    Tile(
      ObsTabTilesIds.FinderChartsId.id,
      s"Finder Charts",
      bodyClass = ExploreStyles.FinderChartsTile.some,
      canMinimize = true
    )(_ => FinderCharts())
