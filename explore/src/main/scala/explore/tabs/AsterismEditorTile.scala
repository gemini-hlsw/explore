// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import crystal.react.ReuseView
import crystal.react.implicits._
import crystal.react.reuse._
import explore.components.Tile
import explore.implicits._
import explore.model.ObsIdSet
import explore.model.TargetVisualOptions
import explore.model.TargetWithId
import explore.model.reusability._
import explore.targeteditor.AsterismEditor
import explore.undo.UndoStacks
import explore.utils._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import react.common._

object AsterismEditorTile {

  def asterismEditorTile(
    userId:        Option[User.Id],
    obsId:         ObsIdSet,
    asterismPot:   Pot[ReuseView[List[TargetWithId]]],
    currentTarget: Option[Target.Id],
    setTarget:     (Option[Target.Id], SetRouteVia) ==> Callback,
    otherObsCount: Target.Id ==> Int,
    undoStacks:    ReuseView[Map[Target.Id, UndoStacks[IO, Target.Sidereal]]],
    searching:     ReuseView[Set[Target.Id]],
    options:       ReuseView[TargetVisualOptions],
    title:         String,
    backButton:    Option[Reuse[VdomNode]] = None,
    hiddenColumns: ReuseView[Set[String]],
    width:         Int,
    height:        Int
  )(implicit ctx:  AppContextIO) =
    Tile(ObsTabTiles.TargetId, title, back = backButton, canMinimize = true)(
      Reuse.by(
        (userId,
         obsId,
         asterismPot,
         currentTarget,
         setTarget,
         otherObsCount,
         undoStacks,
         searching,
         options,
         hiddenColumns,
         width,
         height
        )
      ) { (renderInTitle: Tile.RenderInTitle) =>
        potRender[ReuseView[List[TargetWithId]]](
          (
            (asterism: ReuseView[List[TargetWithId]]) =>
              userId.map(uid =>
                AsterismEditor(uid,
                               obsId,
                               asterism,
                               currentTarget,
                               setTarget,
                               otherObsCount,
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
