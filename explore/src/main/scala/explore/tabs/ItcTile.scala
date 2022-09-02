// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import crystal.react.View
import explore.common.ObsQueries.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits.*
import explore.itc.ItcGraphPanel
import explore.itc.ItcPanelProps
import explore.itc.ItcPanelTitle
import explore.model.ScienceMode
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcTarget
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.given

case class ItcTile(
  scienceMode:              Option[ScienceMode],
  spectroscopyRequirements: Option[SpectroscopyRequirementsData],
  scienceData:              Option[ScienceData],
  itcExposureTime:          Option[ItcChartExposureTime]
) extends ItcPanelProps(scienceMode, spectroscopyRequirements, scienceData, itcExposureTime)

object ItcTile:

  def itcTile(
    scienceMode:              Option[ScienceMode],
    spectroscopyRequirements: Option[SpectroscopyRequirementsData],
    scienceData:              Option[ScienceData],
    itcExposureTime:          Option[ItcChartExposureTime],
    selectedTarget:           View[Option[ItcTarget]]
  )(using AppContextIO) =
    Tile(
      ObsTabTilesIds.ItcId.id,
      s"ITC",
      canMinimize = true,
      control = _ =>
        (ItcPanelTitle(scienceMode,
                       spectroscopyRequirements,
                       scienceData,
                       itcExposureTime,
                       selectedTarget
        ): VdomNode).some,
      bodyClass = ExploreStyles.ItcTileBody.some
    )(_ =>
      ItcGraphPanel(scienceMode,
                    spectroscopyRequirements,
                    scienceData,
                    itcExposureTime,
                    selectedTarget
      )
    )
