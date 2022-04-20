// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.react.ReuseView
import crystal.react.reuse._
import explore.components.Tile
import explore.targeteditor.SiderealTargetEditor
import explore.undo.UndoStacks
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import react.common._

object SiderealTargetEditorTile {

  def sideralTargetEditorTile(
    userId:     Option[User.Id],
    targetId:   Target.Id,
    target:     ReuseView[Target.Sidereal],
    undoStacks: ReuseView[UndoStacks[IO, Target.Sidereal]],
    searching:  ReuseView[Set[Target.Id]],
    title:      String,
    backButton: Option[Reuse[VdomNode]] = None,
    width:      Int,
    height:     Int
  ) =
    Tile(ObsTabTiles.TargetId, title, back = backButton, canMinimize = true)(
      Reuse
        .by(
          (userId, targetId, target, undoStacks, searching, width, height)
        ) { (renderInTitle: Tile.RenderInTitle) =>
          println(title)
          userId.map(uid =>
            SiderealTargetEditor(uid,
                                 targetId,
                                 target,
                                 none,
                                 undoStacks,
                                 searching,
                                 renderInTitle = renderInTitle.some
            )
          ): VdomNode
        }
        .reuseAlways
    )
}
