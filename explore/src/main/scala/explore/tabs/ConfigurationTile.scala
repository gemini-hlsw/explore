// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.AppCtx
import explore.components.Tile
import explore.config.ConfigurationPanel
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.ui.reusability._
import react.common._

object ConfigurationTile {
  def configurationTile(
    obsId: Option[Observation.Id]
  ) = {

    def renderConfiguration(
      obsId:         Option[Observation.Id],
      renderInTitle: Tile.RenderInTitle
    ): VdomNode =
      AppCtx.using { implicit ctx =>
        ConfigurationPanel(obsId, renderInTitle)
      }

    Tile(
      ObsTabTiles.ConfigurationId,
      "Configuration",
      canMinimize = true
    )(
      Reuse(renderConfiguration _)(obsId)
    )
  }

}
