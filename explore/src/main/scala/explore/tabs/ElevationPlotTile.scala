// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.CoordinatesAtVizTime
import explore.model.ScienceMode
import explore.targeteditor.ElevationPlotSection
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Coordinates
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given

import java.time.Instant

object ElevationPlotTile:

  def elevationPlotTile(
    uid:         Option[User.Id],
    tid:         Option[Target.Id],
    scienceMode: Option[ScienceMode],
    coordinates: Option[CoordinatesAtVizTime],
    vizTime:     Option[Instant]
  ) =
    Tile(
      ObsTabTilesIds.PlotId.id,
      "Elevation Plot",
      canMinimize = true,
      bodyClass = ExploreStyles.ElevationPlotTileBody.some
    ) { _ =>
      (uid, tid, coordinates)
        .mapN { (uid, targetId, coordinates) =>
          ElevationPlotSection(uid, targetId, scienceMode, vizTime, coordinates): VdomNode
        }
        .getOrElse {
          <.div(
            ExploreStyles.FullHeightWidth |+| ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
            <.div("Select a target")
          )
        }
    }
