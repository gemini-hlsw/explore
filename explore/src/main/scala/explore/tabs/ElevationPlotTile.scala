// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all._
import crystal.react.reuse._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.targeteditor.ElevationPlotSection
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._

object ElevationPlotTile {

  def elevationPlotTile(
    coreWidth:    Int,
    coreHeight:   Int,
    coordinates:  Option[Coordinates]
  )(implicit ctx: AppContextIO) =
    Tile(
      ObsTabTilesIds.PlotId,
      "Elevation Plot",
      canMinimize = true,
      bodyClass = ExploreStyles.ElevationPlotTileBody.some,
      tileClass = ExploreStyles.ElevationPlotTile.some
    )(
      Reuse
        .by(
          (coreWidth, coreHeight, coordinates)
        ) { (_: Tile.RenderInTitle) =>
          coordinates.fold[VdomNode](
            <.div(
              ExploreStyles.FullHeightWidth |+| ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
              <.div("Select a target")
            )
          )(c => ElevationPlotSection(c))
        }
        .reuseAlways
    )

}
