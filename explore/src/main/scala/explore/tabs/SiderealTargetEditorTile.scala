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
import explore.model.GuideStarSelection
import explore.model.ObsTabTilesIds
import explore.model.ObservationsAndTargets
import explore.model.OnCloneParameters
import explore.model.TargetEditObsInfo
import explore.targeteditor.SiderealTargetEditor
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.schemas.model.TargetWithId
import lucuma.ui.syntax.all.given

object SiderealTargetEditorTile:

  def noObsSiderealTargetEditorTile(
    programId:          Program.Id,
    userId:             Option[User.Id],
    targetId:           Target.Id,
    target:             UndoSetter[Target.Sidereal],
    obsAndTargets:      UndoSetter[ObservationsAndTargets],
    searching:          View[Set[Target.Id]],
    title:              String,
    fullScreen:         View[AladinFullScreen],
    globalPreferences:  View[GlobalPreferences],
    guideStarSelection: View[GuideStarSelection],
    readonly:           Boolean,
    obsInfo:            TargetEditObsInfo,
    onClone:            OnCloneParameters => Callback,
    backButton:         Option[VdomNode] = none
  ) =
    Tile(
      ObsTabTilesIds.TargetId.id,
      title,
      back = backButton,
      bodyClass = ExploreStyles.TargetTileBody
    ) { _ =>
      <.div(
        ExploreStyles.AladinFullScreen.when(fullScreen.get.value),
        <.div(
          ExploreStyles.TargetTileEditor,
          userId.map(uid =>
            SiderealTargetEditor(
              programId,
              uid,
              target,
              obsAndTargets,
              Asterism.one(TargetWithId(targetId, target.get)),
              vizTime = none,
              obsConf = none,
              searching = searching,
              obsInfo = obsInfo,
              onClone = onClone,
              fullScreen = fullScreen,
              globalPreferences = globalPreferences,
              guideStarSelection = guideStarSelection,
              readonly = readonly
            )
          )
        )
      )
    }
