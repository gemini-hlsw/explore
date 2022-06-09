// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import crystal.react.View
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.ObsQueries._
import explore.components.Tile
import explore.config.ConfigurationPanel
import explore.implicits._
import explore.model.ObsConfiguration
import explore.undo._
import explore.utils._
import lucuma.core.model.Observation
import queries.schemas.itc.implicits._
import react.common._

object ConfigurationTile {
  def configurationTile(
    obsId:        Observation.Id,
    obsData:      Pot[(String, Option[NonEmptyString], View[ObservationData])],
    undoStacks:   View[UndoStacks[IO, ObservationData]]
  )(implicit ctx: AppContextIO) =
    Tile(
      ObsTabTilesIds.ConfigurationId,
      "Configuration",
      canMinimize = true
    )(renderInTitle =>
      potRender[(String, Option[NonEmptyString], View[ObservationData])] {
        case (title, subtitle, scienceData) =>
          ConfigurationPanel(
            obsId,
            title,
            subtitle,
            UndoContext(undoStacks, scienceData),
            scienceData.zoom(scienceDataForObs).get.constraints,
            scienceData.zoom(scienceDataForObs).get.itcTargets,
            renderInTitle
          )
      }(obsData)
    )
}
