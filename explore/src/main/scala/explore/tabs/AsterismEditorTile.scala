// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.Pot
import crystal.react.View
import crystal.react.ViewOpt
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.Asterism
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ScienceMode
import explore.targeteditor.AsterismEditor
import explore.undo.UndoStacks
import explore.utils._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import react.common._
import java.time.Instant

object AsterismEditorTile {

  def asterismEditorTile(
    userId:          Option[User.Id],
    programId:       Program.Id,
    obsId:           ObsIdSet,
    potAsterismMode: Pot[(View[Option[Asterism]], Option[ScienceMode])],
    potVizTime:      Pot[View[Option[Instant]]],
    obsConf:         ViewOpt[ObsConfiguration],
    currentTarget:   Option[Target.Id],
    setTarget:       (Option[Target.Id], SetRouteVia) => Callback,
    otherObsCount:   Target.Id => Int,
    undoStacks:      View[Map[Target.Id, UndoStacks[IO, Target.Sidereal]]],
    searching:       View[Set[Target.Id]],
    title:           String,
    backButton:      Option[VdomNode] = none,
    hiddenColumns:   View[Set[String]]
  )(implicit ctx:    AppContextIO) =
    Tile(
      ObsTabTilesIds.TargetId,
      title,
      back = backButton,
      canMinimize = true,
      bodyClass = Some(ExploreStyles.TargetTileBody)
    )((renderInTitle: Tile.RenderInTitle) =>
      potRender[(View[Option[Asterism]], Option[ScienceMode])] {
        (asterismMode: (View[Option[Asterism]], Option[ScienceMode])) =>
          val (asterism, scienceMode) = asterismMode
          userId.map(uid =>
            AsterismEditor(
              uid,
              programId,
              obsId,
              asterism,
              potVizTime,
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
      }(
        potAsterismMode
      )
    )

}
