// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.common.ObsQueries._
import explore.components.Tile
import explore.config.ConfigurationPanel
import explore.implicits._
import explore.model.reusability._
import explore.undo._
import explore.utils.potRender
import lucuma.core.model.Observation
import lucuma.ui.reusability._
import react.common._

object ConfigurationTile {
  def configurationTile(
    obsId:        Observation.Id,
    scienceData:  Pot[View[ScienceData]],
    undoStacks:   View[UndoStacks[IO, ScienceData]]
  )(implicit ctx: AppContextIO) =
    Tile(
      ObsTabTiles.ConfigurationId,
      "Configuration",
      canMinimize = true
    )(
      (scienceData, undoStacks).curryReusing.in((potView, undoStacks_, renderInTitle) =>
        potRender[View[ScienceData]](
          Reuse.always(scienceData_ =>
            ConfigurationPanel(obsId, UndoContext(undoStacks_, scienceData_), renderInTitle)
          )
        )(potView)
      )
    )

}
