// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import explore.components.Tile
import explore.implicits._
import explore.model.ScienceMode
import explore.common.ObsQueries._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.syntax.all.given
import explore.itc.ItcGraphBody

object ItcTile:

  def itcTile(
    scienceMode:              Option[ScienceMode],
    spectroscopyRequirements: Option[SpectroscopyRequirementsData],
    scienceData:              Option[ScienceData]
  )(using AppContextIO) =
    Tile(
      ObsTabTilesIds.ItcId.id,
      s"ITC",
      // props.backButton.some,
      canMinimize = true
    )(_ => ItcGraphBody(scienceMode, spectroscopyRequirements, scienceData))
