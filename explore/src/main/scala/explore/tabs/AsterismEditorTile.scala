// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.*
import crystal.react.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.GlobalPreferences
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ObsTabTilesIds
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.model.OnCloneParameters
import explore.model.TargetEditObsInfo
import explore.model.enums.TileSizeState
import explore.targeteditor.AsterismEditorBody
import explore.targeteditor.AsterismEditorTitle
import explore.targeteditor.AsterismTileState
import explore.undo.UndoSetter
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import queries.schemas.odb.ObsQueries

import java.time.Instant

object AsterismEditorTile:

  def asterismEditorTile(
    userId:            Option[User.Id],
    programId:         Program.Id,
    obsIds:            ObsIdSet,
    obsAndTargets:     UndoSetter[ObservationsAndTargets],
    configuration:     Option[BasicConfiguration],
    vizTime:           View[Option[Instant]],
    vizDuration:       View[Option[TimeSpan]],
    obsConf:           ObsConfiguration,
    pendingTime:       Option[TimeSpan], // estimated remaining execution time.
    currentTarget:     Option[Target.Id],
    setTarget:         (Option[Target.Id], SetRouteVia) => Callback,
    onCloneTarget:     OnCloneParameters => Callback,
    onAsterismUpdate:  OnAsterismUpdateParams => Callback,
    obsInfo:           Target.Id => TargetEditObsInfo,
    searching:         View[Set[Target.Id]],
    title:             String,
    globalPreferences: View[GlobalPreferences],
    readonly:          Boolean,
    sequenceChanged:   Callback = Callback.empty,
    backButton:        Option[VdomNode] = none
  )(using FetchClient[IO, ObservationDB], Logger[IO]): Tile[AsterismTileState] = {
    // Save the time here. this works for the obs and target tabs
    // It's OK to save the viz time for executed observations, I think.
    val vizTimeView =
      vizTime.withOnMod(t => ObsQueries.updateVisualizationTime[IO](obsIds.toList, t).runAsync)

    val vizDurationView =
      vizDuration.withOnMod(t =>
        ObsQueries.updateVisualizationDuration[IO](obsIds.toList, t).runAsync
      )

    Tile(
      ObsTabTilesIds.TargetId.id,
      title,
      AsterismTileState(),
      back = backButton,
      bodyClass = ExploreStyles.TargetTileBody,
      controllerClass = ExploreStyles.TargetTileController
    )(
      a =>
        userId.map(uid =>
          AsterismEditorBody(
            programId,
            uid,
            obsIds,
            obsAndTargets,
            vizTime,
            obsConf,
            currentTarget,
            setTarget,
            onCloneTarget,
            onAsterismUpdate,
            obsInfo,
            searching,
            globalPreferences,
            readonly,
            sequenceChanged,
            a
          )
        ),
      (s, _) =>
        AsterismEditorTitle(programId,
                            obsIds,
                            obsAndTargets,
                            onAsterismUpdate,
                            readonly,
                            vizTimeView,
                            vizDurationView,
                            pendingTime,
                            s
        )
    )
  }
