// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import crystal.Pot
import crystal.react.View
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.itc.ItcPanelBody
import explore.itc.ItcPanelTileState
import explore.itc.ItcPanelTitle
import explore.itc.ItcProps
import explore.model.GlobalPreferences
import explore.model.LoadingState
import explore.model.ObsTabTilesIds
import explore.model.Observation
import explore.model.TargetList
import explore.model.itc.ItcGraphResult
import explore.model.itc.ItcTarget
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.User
import lucuma.ui.syntax.all.given

object ItcTile:

  def itcTile(
    uid:                Option[User.Id],
    oid:                Observation.Id,
    allTargets:         TargetList,
    itcProps:           ItcProps,
    itcGraphResults:    Map[ItcTarget, Pot[ItcGraphResult]],
    itcBrightestTarget: Option[ItcTarget],
    itcLoading:         LoadingState,
    globalPreferences:  View[GlobalPreferences]
  ) =
    Tile(
      ObsTabTilesIds.ItcId.id,
      s"ITC",
      ItcPanelTileState(),
      bodyClass = ExploreStyles.ItcTileBody
    )(
      s =>
        uid.map(
          ItcPanelBody(
            _,
            oid,
            itcProps,
            itcGraphResults,
            itcBrightestTarget,
            itcLoading,
            globalPreferences,
            s
          )
        ),
      (s, _) =>
        ItcPanelTitle(
          itcProps,
          itcGraphResults,
          itcLoading,
          s
        )
    )
