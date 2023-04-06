// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import crystal.react.View
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.Tile
import explore.config.ConfigurationPanel
import explore.model.BasicConfigAndItc
import explore.model.ObsConfiguration
import explore.model.TargetList
import explore.model.enums.AgsState
import explore.undo.*
import explore.utils.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import queries.schemas.itc.ITCConversions.*
import queries.schemas.odb.ObsQueries.*

object ConfigurationTile {
  def configurationTile(
    userId:          Option[User.Id],
    programId:       Program.Id,
    obsId:           Observation.Id,
    obsData:         Pot[(String, Option[NonEmptyString], View[ScienceData])],
    undoStacks:      View[UndoStacks[IO, ScienceData]],
    baseCoordinates: Option[CoordinatesAtVizTime],
    obsConf:         Option[ObsConfiguration],
    selectedConfig:  View[Option[BasicConfigAndItc]],
    allTargets:      TargetList
  )(using Logger[IO]) =
    Tile(
      ObsTabTilesIds.ConfigurationId.id,
      "Configuration",
      canMinimize = true
    )(renderInTitle =>
      obsData.renderPot((title, subtitle, scienceData) =>
        ConfigurationPanel(
          userId,
          programId,
          obsId,
          title,
          subtitle,
          UndoContext(undoStacks, scienceData),
          obsConf,
          scienceData.get.itcTargets(allTargets),
          baseCoordinates,
          selectedConfig,
          renderInTitle
        )
      )
    )
}
