// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import explore.common.ObsQueries._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.itc.ItcGraphPanel
import explore.model.ScienceMode
import explore.model.itc.ItcChartExposureTime
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.syntax.all.given

object ItcTile:

  def itcTile(
    scienceMode:              Option[ScienceMode],
    spectroscopyRequirements: Option[SpectroscopyRequirementsData],
    scienceData:              Option[ScienceData],
    itcExposureTime:          Option[ItcChartExposureTime]
  )(using AppContextIO) =
    Tile(
      ObsTabTilesIds.ItcId.id,
      s"ITC",
      canMinimize = true,
      bodyClass = ExploreStyles.ItcTileBody.some
    )(_ => ItcGraphPanel(scienceMode, spectroscopyRequirements, scienceData, itcExposureTime))
