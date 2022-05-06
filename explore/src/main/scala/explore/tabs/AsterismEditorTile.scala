// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import crystal.react.ReuseView
import crystal.react.implicits._
import crystal.react.reuse._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ScienceModeBasic
import explore.model.TargetWithId
import explore.model.reusability._
import explore.targeteditor.AsterismEditor
import explore.undo.UndoStacks
import explore.utils._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import react.common._

object AsterismEditorTile {

  def asterismEditorTile(
    userId:          Option[User.Id],
    programId:       Program.Id,
    obsId:           ObsIdSet,
    potAsterismMode: Pot[(ReuseView[List[TargetWithId]], Option[ScienceModeBasic])],
    obsConf:         Option[ObsConfiguration],
    currentTarget:   Option[Target.Id],
    setTarget:       (Option[Target.Id], SetRouteVia) ==> Callback,
    otherObsCount:   Target.Id ==> Int,
    undoStacks:      ReuseView[Map[Target.Id, UndoStacks[IO, Target.Sidereal]]],
    searching:       ReuseView[Set[Target.Id]],
    title:           String,
    backButton:      Option[Reuse[VdomNode]] = None,
    hiddenColumns:   ReuseView[Set[String]],
    width:           Int,
    height:          Int
  )(implicit ctx:    AppContextIO) =
    Tile(
      ObsTabTiles.TargetId,
      title,
      back = backButton,
      canMinimize = true,
      bodyClass = Some(ExploreStyles.TargetTileBody)
    )(
      Reuse.by(
        (userId,
         programId,
         obsId,
         potAsterismMode,
         obsConf,
         currentTarget,
         setTarget,
         otherObsCount,
         undoStacks,
         searching,
         hiddenColumns,
         width,
         height
        )
      ) { (renderInTitle: Tile.RenderInTitle) =>
        potRender[(ReuseView[List[TargetWithId]], Option[ScienceModeBasic])](
          { (asterismMode: (ReuseView[List[TargetWithId]], Option[ScienceModeBasic])) =>
            val (asterism, scienceMode) = asterismMode
            userId.map(uid =>
              AsterismEditor(
                uid,
                programId,
                obsId,
                asterism,
                scienceMode,
                obsConf,
                currentTarget,
                setTarget,
                otherObsCount,
                undoStacks,
                searching,
                hiddenColumns,
                renderInTitle
              )
            ): VdomNode
          }.reuseAlways
        )(
          potAsterismMode
        )
      }
    )

}
