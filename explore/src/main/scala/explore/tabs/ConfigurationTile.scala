// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import crystal.react.ReuseView
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.common.ObsQueries._
import explore.components.Tile
import explore.config.ConfigurationPanel
import explore.implicits._
import queries.schemas.itc.implicits._
import explore.undo._
import explore.utils.potRender
import lucuma.core.model.Observation
import lucuma.ui.reusability._
import react.common._

object ConfigurationTile {
  def configurationTile(
    obsId:        Observation.Id,
    scienceData:  Pot[ReuseView[ScienceData]],
    undoStacks:   ReuseView[UndoStacks[IO, ScienceData]]
  )(implicit ctx: AppContextIO) =
    Tile(
      ObsTabTiles.ConfigurationId,
      "Configuration",
      canMinimize = true
    )(
      (scienceData, undoStacks).curryReusing.in((potView, undoStacks_, renderInTitle) =>
        potRender[ReuseView[ScienceData]](
          Reuse.always(scienceData_ =>
            ConfigurationPanel(obsId,
                               scienceData_.map(UndoContext(undoStacks_, _)),
                               scienceData_.get.constraints,
                               scienceData_.get.itcTargets,
                               renderInTitle
            )
          )
        )(potView)
      )
    )

}
