// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all._
import crystal.react.reuse._
import explore.components.Tile
import explore.implicits._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.reusability._
import react.common._
import explore.components.ui.ExploreStyles
import explore.targeteditor.SkyPlotSection
import lucuma.core.math.Coordinates

object ElevationPlotTile {

  def elevationPlotTile(
    coreWidth:    Int,
    coreHeight:   Int,
    coordinates:  Coordinates
  )(implicit ctx: AppContextIO) =
    Tile(ObsTabTiles.PlotId,
         "Elevation Plot",
         canMinimize = true,
         bodyClass = ExploreStyles.SkyPlotTileBody.some,
         tileClass = ExploreStyles.SkyPlotTile.some
    )(
      Reuse
        .by(
          (coreWidth, coreHeight, coordinates)
        ) { (_: Tile.RenderInTitle) =>
          SkyPlotSection(coordinates): VdomNode
        }
        .reuseAlways
    )

}
