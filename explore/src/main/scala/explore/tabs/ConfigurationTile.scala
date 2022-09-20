// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import crystal.react.View
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.ObsQueries.*
import explore.components.Tile
import explore.config.ConfigurationPanel
import explore.implicits.*
import explore.model.CoordinatesAtVizTime
import explore.undo.*
import explore.utils.*
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.schemas.itc.implicits.*

object ConfigurationTile {
  def configurationTile(
    obsId:           Observation.Id,
    obsData:         Pot[(String, Option[NonEmptyString], View[ScienceData])],
    undoStacks:      View[UndoStacks[IO, ScienceData]],
    baseCoordinates: Option[CoordinatesAtVizTime]
  )(using AppContextIO) =
    Tile(
      ObsTabTilesIds.ConfigurationId.id,
      "Configuration",
      canMinimize = true
    )(renderInTitle =>
      potRender[(String, Option[NonEmptyString], View[ScienceData])] {
        (title, subtitle, scienceData) =>
          ConfigurationPanel(
            obsId,
            title,
            subtitle,
            UndoContext(undoStacks, scienceData),
            scienceData.get.constraints,
            scienceData.get.itcTargets,
            baseCoordinates,
            renderInTitle
          )
      }(obsData)
    )
}
