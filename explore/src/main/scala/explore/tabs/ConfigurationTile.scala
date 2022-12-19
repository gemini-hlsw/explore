// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import crystal.react.View
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.Tile
import explore.config.ConfigurationPanel
import explore.model.CoordinatesAtVizTime
import explore.undo.*
import explore.utils.*
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import queries.schemas.itc.ITCConversions.*
import queries.schemas.odb.ObsQueries.*
import explore.model.enums.AgsState

object ConfigurationTile {
  def configurationTile(
    userId:          Option[User.Id],
    obsId:           Observation.Id,
    obsData:         Pot[(String, Option[NonEmptyString], View[ScienceData])],
    undoStacks:      View[UndoStacks[IO, ScienceData]],
    baseCoordinates: Option[CoordinatesAtVizTime],
    agsState: AgsState
  )(using Logger[IO]) =
    Tile(
      ObsTabTilesIds.ConfigurationId.id,
      "Configuration",
      canMinimize = true
    )(renderInTitle =>
      potRender[(String, Option[NonEmptyString], View[ScienceData])] {
        (title, subtitle, scienceData) =>
          ConfigurationPanel(
            userId,
            obsId,
            title,
            subtitle,
            UndoContext(undoStacks, scienceData),
            scienceData.get.constraints,
            scienceData.get.itcTargets,
            baseCoordinates,
            agsState,
            renderInTitle
          )
      }(obsData)
    )
}
