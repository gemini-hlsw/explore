// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationPanel
import explore.model.AsterismIds
import explore.model.BasicConfigAndItc
import explore.model.ObsConfiguration
import explore.model.ObsSummary.scienceTargetIds
import explore.model.ScienceRequirements
import explore.model.TargetList
import explore.model.enums.AgsState
import explore.undo.*
import explore.utils.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.schemas.model.ObservingMode
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import queries.schemas.itc.syntax.*
import queries.schemas.odb.ObsQueries.*

object ConfigurationTile {
  def configurationTile(
    userId:           Option[User.Id],
    programId:        Program.Id,
    obsId:            Observation.Id,
    title:            String,
    subtitle:         Option[NonEmptyString],
    requirements:     UndoSetter[ScienceRequirements],
    mode:             UndoSetter[Option[ObservingMode]],
    posAngle:         View[PosAngleConstraint],
    scienceTargetIds: AsterismIds,
    baseCoordinates:  Option[CoordinatesAtVizTime],
    obsConf:          ObsConfiguration,
    selectedConfig:   View[Option[BasicConfigAndItc]],
    allTargets:       TargetList
  )(using Logger[IO]) =
    Tile(
      ObsTabTilesIds.ConfigurationId.id,
      "Configuration",
      bodyClass = ExploreStyles.ConfigurationTileBody.some,
      canMinimize = true
    )(_ =>
      ConfigurationPanel(
        userId,
        programId,
        obsId,
        title,
        subtitle,
        requirements,
        mode,
        posAngle,
        obsConf,
        scienceTargetIds.itcTargets(allTargets),
        baseCoordinates,
        selectedConfig
      )
    )
}
