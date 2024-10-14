// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.GlobalPreferences
import explore.model.ObsTabTilesIds
import explore.targeteditor.ElevationPlotSection
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Site
import lucuma.core.model.ObjectTracking
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.User
import lucuma.ui.syntax.all.given

import java.time.Duration
import java.time.Instant
import explore.targeteditor.ElevationPlotData

object ElevationPlotTile:

  def elevationPlotTile(
    userId:            Option[User.Id],
    plotData:          ElevationPlotData,
    site:              Option[Site],
    vizTime:           Option[Instant],
    pendingTime:       Option[Duration],
    timingWindows:     List[TimingWindow] = List.empty,
    globalPreferences: GlobalPreferences
  ): Tile[Unit] =
    Tile(
      ObsTabTilesIds.PlotId.id,
      "Elevation Plot",
      bodyClass = ExploreStyles.ElevationPlotTileBody
    ) { _ =>
      userId
        .map: uid =>
          ElevationPlotSection(
            uid,
            plotData,
            site,
            vizTime,
            pendingTime,
            timingWindows,
            globalPreferences
          ): VdomNode
        .getOrElse:
          <.div(
            ExploreStyles.FullHeightWidth |+| ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
            <.div("Select a target")
          )
    }
