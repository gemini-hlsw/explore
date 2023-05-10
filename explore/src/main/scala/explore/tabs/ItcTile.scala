// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.itc.ItcGraphPanel
import explore.itc.ItcPanelProps
import explore.itc.ItcPanelTitle
import explore.model.BasicConfigAndItc
import explore.model.LoadingState
import explore.model.TargetList
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcChartResult
import explore.model.itc.ItcTarget
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.ui.syntax.all.given

object ItcTile:

  def itcTile(
    uid:             Option[User.Id],
    oid:             Observation.Id,
    itcExposureTime: Option[ItcChartExposureTime],
    selectedConfig:  Option[BasicConfigAndItc],
    selectedTarget:  View[Option[ItcTarget]],
    allTargets:      TargetList,
    itcProps:        ItcPanelProps,
    itcChartResults: Pot[Map[ItcTarget, ItcChartResult]],
    itcLoading:      LoadingState
  ) =
    Tile(
      ObsTabTilesIds.ItcId.id,
      s"ITC",
      canMinimize = true,
      control = _ =>
        (ItcPanelTitle(
          selectedTarget,
          allTargets,
          itcProps,
          itcChartResults,
          itcLoading
        ): VdomNode).some,
      bodyClass = ExploreStyles.ItcTileBody.some
    )(_ =>
      uid.map(
        ItcGraphPanel(
          _,
          oid,
          selectedTarget,
          selectedConfig,
          itcProps,
          itcChartResults,
          itcLoading
        )
      )
    )
