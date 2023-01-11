// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import crystal.Pot
import crystal.react.View
import crystal.react.implicits.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.config.VizTimeEditor
import explore.model.Asterism
import explore.model.ObsIdSet
import explore.model.PAProperties
import explore.model.ScienceMode
import explore.model.enums.AgsState
import explore.model.enums.TileSizeState
import explore.targeteditor.AsterismEditor
import explore.undo.UndoStacks
import explore.utils.*
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.schemas.ObservationDB
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import queries.schemas.odb.ObsQueries
import react.common.ReactFnProps

import java.time.Instant

object AsterismEditorTile:

  def asterismEditorTile(
    userId:          Option[User.Id],
    programId:       Program.Id,
    sharedInObsIds:  ObsIdSet,
    potAsterismMode: Pot[(View[Option[Asterism]], Option[ScienceMode])],
    potVizTime:      Pot[View[Option[Instant]]],
    constraints:     Option[ConstraintSet],
    wavelength:      Option[Wavelength],
    currentTarget:   Option[Target.Id],
    setTarget:       (Option[Target.Id], SetRouteVia) => Callback,
    otherObsCount:   Target.Id => Int,
    undoStacks:      View[Map[Target.Id, UndoStacks[IO, Target.Sidereal]]],
    searching:       View[Set[Target.Id]],
    title:           String,
    paProps:         Option[PAProperties],
    backButton:      Option[VdomNode] = none
  )(using TransactionalClient[IO, ObservationDB], Logger[IO]): Tile = {
    // Save the time here. this works for the obs and target tabs
    val vizTimeView = potVizTime.map(_.withOnMod { t =>
      ObsQueries.updateVisualizationTime[IO](programId, sharedInObsIds.toList, t).runAsync
    })

    // Store the pos angle on the db
    val posAngleView = paProps.map { case p @ PAProperties(oid, _, agsStateView, paView) =>
      p.copy(constraint =
        paView.withOnMod(pa =>
          agsStateView.set(AgsState.Saving) *> ObsQueries
            .updatePosAngle[IO](programId, List(oid), pa)
            .guarantee(agsStateView.async.set(AgsState.Idle))
            .runAsync
        )
      )
    }

    def control: VdomNode = <.div(VizTimeEditor(vizTimeView))

    Tile(
      ObsTabTilesIds.TargetId.id,
      title,
      back = backButton,
      canMinimize = true,
      control = s => control.some.filter(_ => s === TileSizeState.Minimized),
      bodyClass = Some(ExploreStyles.TargetTileBody)
    )((renderInTitle: Tile.RenderInTitle) =>
      potAsterismMode.render((asterism, scienceMode) =>
        userId.map(uid =>
          AsterismEditor(
            uid,
            programId,
            sharedInObsIds,
            asterism,
            potVizTime,
            scienceMode,
            constraints,
            wavelength,
            currentTarget,
            setTarget,
            otherObsCount,
            undoStacks,
            searching,
            renderInTitle,
            posAngleView
          )
        )
      )
    )
  }
