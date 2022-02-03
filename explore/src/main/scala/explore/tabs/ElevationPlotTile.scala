// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all._
import crystal.react._
import crystal.react.reuse._
import crystal.react.implicits._
import explore.components.Tile
import explore.implicits._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.core.model.User
import explore.model.reusability._
import lucuma.ui.reusability._
import react.common._
import explore.components.ui.ExploreStyles
import explore.targeteditor.SkyPlotSection

object ElevationPlotTile {

  def elevationPlotTile(
    userId:         Option[User.Id],
    coreWidth:      Int,
    coreHeight:     Int,
    selectedTarget: Option[ViewOpt[Target]]
  )(implicit ctx:   AppContextIO) =
    Tile(ObsTabTiles.PlotId,
         "Elevation Plot",
         canMinimize = true,
         bodyClass = ExploreStyles.SkyPlotTileBody.some,
         tileClass = ExploreStyles.SkyPlotTile.some
    )(
      Reuse
        .by(
          (userId, coreWidth, coreHeight, selectedTarget)
        ) { (_: Tile.RenderInTitle) =>
          selectedTarget
            .flatMap(
              _.mapValue(targetView =>
                targetView.get match {
                  case t @ Target.Sidereal(_, _, _, _) =>
                    val baseCoordinates =
                      Target.Sidereal.baseCoordinates.get(t)
                    SkyPlotSection(baseCoordinates): VdomNode
                  case _                               => EmptyVdom
                }
              )
            )
            .getOrElse(EmptyVdom)
        }
        .reuseAlways
    )

}
