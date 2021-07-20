// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.AppCtx
import explore.common.ObsQueries._
import explore.components.Tile
import explore.config.ConfigurationPanel
import explore.implicits._
import explore.model.reusability._
import explore.undo._
import explore.utils.potRender
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.ui.reusability._
import react.common._

object ConfigurationTile {
  def configurationTile(
    obsId:               Observation.Id,
    scienceRequirements: Pot[View[ScienceRequirementsData]],
    undoStacks:          View[UndoStacks[IO, ScienceRequirementsData]]
  ) = {

    def renderConfiguration(
      obsId:               Observation.Id,
      scienceRequirements: View[ScienceRequirementsData],
      undoStacks:          View[UndoStacks[IO, ScienceRequirementsData]],
      renderInTitle:       Tile.RenderInTitle
    ): VdomNode =
      AppCtx.using { implicit ctx =>
        ConfigurationPanel(obsId, scienceRequirements, undoStacks, renderInTitle)
      }

    Tile(
      ObsTabTiles.ConfigurationId,
      "Configuration",
      canMinimize = true
    )(
      (scienceRequirements, undoStacks).curryReusing.in((potView_, undoStacks_, renderInTitle) =>
        potRender[View[ScienceRequirementsData]](
          Reuse.always(cs => renderConfiguration(obsId, cs, undoStacks_, renderInTitle))
        )(potView_)
      )
    )
  }

}
