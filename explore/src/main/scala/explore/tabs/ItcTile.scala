// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import crystal.react.View
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.itc.ItcGraphPanel
import explore.itc.ItcPanelProps
import explore.itc.ItcPanelTitle
import explore.model.BasicConfigAndItc
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcTarget
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.schemas.model.ObservingMode
import lucuma.ui.syntax.all.given
import queries.schemas.odb.ObsQueries.*

object ItcTile:

  def itcTile(
    uid:                      Option[User.Id],
    oid:                      Observation.Id,
    observingMode:            Option[ObservingMode],
    spectroscopyRequirements: Option[SpectroscopyRequirementsData],
    scienceData:              Option[ScienceData],
    itcExposureTime:          Option[ItcChartExposureTime],
    selectedTarget:           View[Option[ItcTarget]],
    selectedConfig:           Option[BasicConfigAndItc]
  ) =
    Tile(
      ObsTabTilesIds.ItcId.id,
      s"ITC",
      canMinimize = true,
      control = _ =>
        (ItcPanelTitle(
          observingMode,
          spectroscopyRequirements,
          scienceData,
          itcExposureTime,
          selectedTarget,
          selectedConfig
        ): VdomNode).some,
      bodyClass = ExploreStyles.ItcTileBody.some
    )(_ =>
      uid.map(
        ItcGraphPanel(
          _,
          oid,
          observingMode,
          spectroscopyRequirements,
          scienceData,
          itcExposureTime,
          selectedTarget,
          selectedConfig
        )
      )
    )
