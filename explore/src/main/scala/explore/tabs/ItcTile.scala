// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.itc.ItcGraphPanel
import explore.itc.ItcPanelTitle
import explore.itc.ItcProps
import explore.model.GlobalPreferences
import explore.model.LoadingState
import explore.model.ObsTabTilesIds
import explore.model.TargetList
import explore.model.itc.ItcChartResult
import explore.model.itc.ItcTarget
import japgolly.scalajs.react.vdom.html_<^.*
import explore.model.Observation
import lucuma.core.model.User
import lucuma.ui.syntax.all.given

object ItcTile:

  def itcTile(
    uid:               Option[User.Id],
    oid:               Observation.Id,
    selectedTarget:    View[Option[ItcTarget]],
    allTargets:        TargetList,
    itcProps:          ItcProps,
    itcChartResults:   Map[ItcTarget, Pot[ItcChartResult]],
    itcLoading:        LoadingState,
    globalPreferences: View[GlobalPreferences]
  ) =
    Tile(
      ObsTabTilesIds.ItcId.id,
      s"ITC",
      canMinimize = true,
      control = _ =>
        (ItcPanelTitle(
          selectedTarget,
          itcProps,
          itcChartResults,
          itcLoading
        ): VdomNode).some,
      bodyClass = ExploreStyles.ItcTileBody
    )(_ =>
      uid.map(
        ItcGraphPanel(
          _,
          oid,
          selectedTarget,
          itcProps,
          itcChartResults,
          itcLoading,
          globalPreferences
        )
      )
    )
