// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.Pot
import crystal.react.View
import crystal.react.implicits._
import explore.common.ObsQueries
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.config.VizTimeEditor
import explore.implicits._
import explore.model.Asterism
import explore.model.ObsIdSet
import explore.model.ScienceMode
import explore.model.enums.TileSizeState
import explore.targeteditor.AsterismEditor
import explore.undo.UndoStacks
import explore.utils._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps

import java.time.Instant

object AsterismEditorTile {

  def asterismEditorTile(
    userId:          Option[User.Id],
    programId:       Program.Id,
    obsId:           ObsIdSet,
    potAsterismMode: Pot[(View[Option[Asterism]], Option[ScienceMode])],
    potVizTime:      Pot[View[Option[Instant]]],
    posAngle:        Option[PosAngleConstraint],
    constraints:     Option[ConstraintSet],
    wavelength:      Option[Wavelength],
    currentTarget:   Option[Target.Id],
    setTarget:       (Option[Target.Id], SetRouteVia) => Callback,
    otherObsCount:   Target.Id => Int,
    undoStacks:      View[Map[Target.Id, UndoStacks[IO, Target.Sidereal]]],
    searching:       View[Set[Target.Id]],
    title:           String,
    backButton:      Option[VdomNode] = none,
    hiddenColumns:   View[Set[String]]
  )(implicit ctx:    AppContextIO) = {

    // Save the time here. this works for the obs and target tabs
    val vizTimeView = potVizTime.map(_.withOnMod { t =>
      ObsQueries.updateVisualizationTime[IO](obsId.toList, t).runAsync
    })

    val control: VdomNode = VizTimeEditor(vizTimeView)

    Tile(
      ObsTabTilesIds.TargetId.id,
      title,
      back = backButton,
      canMinimize = true,
      control = s => control.some.filter(_ => s === TileSizeState.Minimized),
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
              posAngle,
              constraints,
              wavelength,
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

}
