// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.Order.given
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AladinFullScreen
import explore.model.Asterism
import explore.model.extensions.*
import explore.model.util.*
import explore.targeteditor.SiderealTargetEditor
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.schemas.model.TargetWithId
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.std.option.some

import java.time.Instant
import scala.collection.immutable.SortedMap

object SiderealTargetEditorTile {

  def noObsSiderealTargetEditorTile(
    userId:     Option[User.Id],
    targetId:   Target.Id,
    target:     UndoSetter[Target.Sidereal],
    searching:  View[Set[Target.Id]],
    title:      String,
    fullScreen: View[AladinFullScreen],
    backButton: Option[VdomNode] = none
  ) =
    Tile(
      ObsTabTilesIds.TargetId.id,
      title,
      back = backButton,
      canMinimize = true,
      bodyClass = Some(ExploreStyles.TargetTileBody)
    ) { (renderInTitle: Tile.RenderInTitle) =>
      <.div(
        ExploreStyles.AladinFullScreen.when(fullScreen.get.value),
        <.div(
          ExploreStyles.TargetTileEditor,
          userId.map(uid =>
            SiderealTargetEditor(
              uid,
              targetId,
              target,
              Asterism.one(TargetWithId(targetId, target.get)).some,
              none,
              none,
              searching,
              renderInTitle = renderInTitle.some,
              fullScreen = fullScreen
            )
          )
        )
      )
    }
}
