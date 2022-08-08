// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import explore.components.Tile
import explore.targeteditor.SiderealTargetEditor
import explore.undo.UndoStacks
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given

import java.time.Instant

object SiderealTargetEditorTile {

  def sideralTargetEditorTile(
    userId:     Option[User.Id],
    targetId:   Target.Id,
    target:     View[Target.Sidereal],
    vizTime:    Option[Instant],
    undoStacks: View[UndoStacks[IO, Target.Sidereal]],
    searching:  View[Set[Target.Id]],
    title:      String,
    fullScreen: View[Boolean],
    backButton: Option[VdomNode] = none
  ) =
    Tile(ObsTabTilesIds.TargetId.id, title, back = backButton, canMinimize = true) {
      (renderInTitle: Tile.RenderInTitle) =>
        userId.map(uid =>
          SiderealTargetEditor(
            uid,
            targetId,
            target,
            vizTime,
            none,
            none,
            none,
            none,
            undoStacks,
            searching,
            renderInTitle = renderInTitle.some,
            fullScreen = fullScreen
          )
        ): VdomNode
    }
}
