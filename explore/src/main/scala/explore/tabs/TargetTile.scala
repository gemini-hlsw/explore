// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.Pot
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ObsIdSet
import explore.model.TargetVisualOptions
import explore.model.TargetWithId
import explore.model.reusability._
import explore.targeteditor.AsterismEditor
import explore.undo.UndoStacks
import explore.utils._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import react.common._

object TargetTile {

  def targetTile(
    userId:        Option[User.Id],
    obsId:         ObsIdSet,
    asterismPot:   Pot[View[List[TargetWithId]]],
    undoStacks:    View[Map[Target.Id, UndoStacks[IO, Target.Sidereal]]],
    searching:     View[Set[Target.Id]],
    options:       View[TargetVisualOptions],
    title:         String,
    backButton:    Option[Reuse[VdomNode]] = None,
    hiddenColumns: View[Set[String]],
    width:         Int,
    height:        Int
  )(implicit ctx:  AppContextIO) =
    Tile(ObsTabTiles.TargetId,
         title,
         back = backButton,
         canMinimize = true,
         bodyClass = ExploreStyles.TargetTileBody.some
    )(
      Reuse.by(
        (userId, obsId, asterismPot, undoStacks, searching, options, hiddenColumns, width, height)
      ) { (renderInTitle: Tile.RenderInTitle) =>
        potRender[View[List[TargetWithId]]](
          (
            (asterism: View[List[TargetWithId]]) =>
              userId.map(uid =>
                AsterismEditor(uid,
                               obsId,
                               asterism,
                               undoStacks,
                               searching,
                               options,
                               hiddenColumns,
                               renderInTitle
                )
              ): VdomNode
          ).reuseAlways
        )(
          asterismPot
        )
      }
    )

}
