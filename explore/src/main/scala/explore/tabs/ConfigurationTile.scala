// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationPanel
import explore.model.AsterismIds
import explore.model.BasicConfigAndItc
import explore.model.ObsConfiguration
import explore.model.ObsTabTilesIds
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.model.TargetList
import explore.modes.SpectroscopyModesMatrix
import explore.undo.*
import japgolly.scalajs.react.Callback
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.schemas.model.ObservingMode
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import queries.schemas.itc.syntax.*

object ConfigurationTile:
  def configurationTile(
    userId:           Option[User.Id],
    programId:        Program.Id,
    obsId:            Observation.Id,
    requirements:     UndoSetter[ScienceRequirements],
    mode:             UndoSetter[Option[ObservingMode]],
    posAngle:         View[PosAngleConstraint],
    scienceTargetIds: AsterismIds,
    baseCoordinates:  Option[CoordinatesAtVizTime],
    obsConf:          ObsConfiguration,
    selectedConfig:   View[Option[BasicConfigAndItc]],
    modes:            SpectroscopyModesMatrix,
    allTargets:       TargetList,
    sequenceChanged:  Callback,
    readonly:         Boolean
  )(using Logger[IO]) =
    Tile(
      ObsTabTilesIds.ConfigurationId.id,
      (),
      "Configuration",
      bodyClass = ExploreStyles.ConfigurationTileBody
    )(_ =>
      ConfigurationPanel(
        userId,
        programId,
        obsId,
        requirements,
        mode,
        posAngle,
        obsConf,
        scienceTargetIds.itcTargets(allTargets),
        baseCoordinates,
        selectedConfig,
        modes,
        sequenceChanged,
        readonly
      )
    )
