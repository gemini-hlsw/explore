// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AladinFullScreen
import explore.model.Asterism
import explore.model.util.*
import explore.targeteditor.SiderealTargetEditor
import explore.undo.UndoStacks
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.std.option.some

import java.time.Instant
import explore.model.enums.AgsState
import lucuma.core.model.Observation

object SiderealTargetEditorTile {

  def siderealTargetEditorTile(
    userId:     Option[User.Id],
    targetId:   Target.Id,
    target:     View[Target.Sidereal],
    vizTime:    Option[Instant],
    undoStacks: View[UndoStacks[IO, Target.Sidereal]],
    searching:  View[Set[Target.Id]],
    title:      String,
    fullScreen: View[AladinFullScreen],
    oidAndAgs:  Option[(Observation.Id, View[AgsState])],
    backButton: Option[VdomNode] = none
  ) =
    Tile(ObsTabTilesIds.TargetId.id,
         title,
         back = backButton,
         canMinimize = true,
         bodyClass = Some(ExploreStyles.TargetTileBody)
    ) { (renderInTitle: Tile.RenderInTitle) =>
      val asterism = target.widen[Target].zoom(Asterism.oneTarget(targetId).reverse.asLens)
      <.div(
        ExploreStyles.AladinFullScreen.when(fullScreen.get.value),
        <.div(
          ExploreStyles.TargetTileEditor,
          userId.map(uid =>
            SiderealTargetEditor(
              uid,
              asterism,
              vizTime,
              none,
              none,
              none,
              none,
              undoStacks,
              searching,
              agsState = oidAndAgs.map(_._2),
              renderInTitle = renderInTitle.some,
              fullScreen = fullScreen
            )
          )
        )
      )
    }
}
