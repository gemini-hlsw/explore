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
    obsId:       Observation.Id,
    scienceData: Pot[View[ScienceData]],
    undoStacks:  View[UndoStacks[IO, ScienceData]]
  ) = {

    def renderConfiguration(
      obsId:         Observation.Id,
      scienceData:   View[ScienceData],
      undoStacks:    View[UndoStacks[IO, ScienceData]],
      renderInTitle: Tile.RenderInTitle
    ): VdomNode =
      AppCtx.using { implicit ctx =>
        ConfigurationPanel(obsId, scienceData, undoStacks, renderInTitle)
      }

    Tile(
      ObsTabTiles.ConfigurationId,
      "Configuration",
      canMinimize = true
    )(
      (scienceData, undoStacks).curryReusing.in((potView, undoStacks_, renderInTitle) =>
        potRender[View[ScienceData]](
          Reuse.always( scienceData_ => renderConfiguration(obsId, scienceData_, undoStacks_, renderInTitle) )
        )(potView)
      )
    )
  }

}
