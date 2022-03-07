// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.TargetVisualOptions
import explore.model.reusability._
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
    target:     View[Target.Sidereal],
    undoStacks: View[UndoStacks[IO, Target.Sidereal]],
    searching:  View[Set[Target.Id]],
    options:    View[TargetVisualOptions],
    title:      String,
    backButton: Option[Reuse[VdomNode]] = None,
    width:      Int,
    height:     Int
  ) =
    Tile(ObsTabTiles.TargetId,
         title,
         back = backButton,
         canMinimize = true,
         bodyClass = ExploreStyles.SiderealTargetEditorTileBody.some
    )(
      Reuse
        .by(
          (userId, targetId, target, undoStacks, searching, options, width, height)
        ) { (renderInTitle: Tile.RenderInTitle) =>
          userId.map(uid =>
            SiderealTargetEditor(uid,
                                 targetId,
                                 target,
                                 undoStacks,
                                 searching,
                                 options,
                                 renderInTitle = renderInTitle.some
            )
          ): VdomNode
        }
        .reuseAlways
    )
}
