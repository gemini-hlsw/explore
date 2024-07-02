// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import crystal.react.View
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AladinFullScreen
import explore.model.Asterism
import explore.model.GlobalPreferences
import explore.model.ObsIdSet
import explore.model.ObsTabTilesIds
import explore.model.TargetEditObsInfo
import explore.targeteditor.SiderealTargetEditor
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.schemas.model.TargetWithId
import lucuma.ui.syntax.all.given

object SiderealTargetEditorTile {

  def noObsSiderealTargetEditorTile(
    userId:            Option[User.Id],
    targetId:          Target.Id,
    target:            UndoSetter[Target.Sidereal],
    searching:         View[Set[Target.Id]],
    title:             String,
    fullScreen:        View[AladinFullScreen],
    globalPreferences: View[GlobalPreferences],
    readonly:          Boolean,
    obsInfo:           TargetEditObsInfo,
    onClone:           (Target.Id, TargetWithId, ObsIdSet) => Callback,
    backButton:        Option[VdomNode] = none
  ) =
    Tile(
      ObsTabTilesIds.TargetId.id,
      title,
      back = backButton,
      canMinimize = true,
      bodyClass = ExploreStyles.TargetTileBody
    ) { (renderInTitle: Tile.RenderInTitle) =>
      <.div(
        ExploreStyles.AladinFullScreen.when(fullScreen.get.value),
        <.div(
          ExploreStyles.TargetTileEditor,
          userId.map(uid =>
            SiderealTargetEditor(
              uid,
              target,
              Asterism.one(TargetWithId(targetId, target.get)),
              vizTime = none,
              obsConf = none,
              searching = searching,
              obsInfo = obsInfo,
              onClone = onClone,
              renderInTitle = renderInTitle.some,
              fullScreen = fullScreen,
              globalPreferences = globalPreferences,
              readonly = readonly
            )
          )
        )
      )
    }
}
