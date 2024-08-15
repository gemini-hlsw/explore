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
import explore.config.ObsTimeEditor
import explore.model.GlobalPreferences
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ObsTabTilesIds
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.model.OnCloneParameters
import explore.model.TargetEditObsInfo
import explore.model.enums.TileSizeState
import explore.targeteditor.AsterismEditor
import explore.undo.UndoSetter
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import queries.schemas.odb.ObsQueries

import java.time.Instant

object AsterismEditorTile

// def asterismEditorTile(
//   userId:            Option[User.Id],
//   programId:         Program.Id,
//   obsIds:            ObsIdSet,
//   obsAndTargets:     UndoSetter[ObservationsAndTargets],
//   configuration:     Option[BasicConfiguration],
//   vizTime:           View[Option[Instant]],
//   obsConf:           ObsConfiguration,
//   currentTarget:     Option[Target.Id],
//   setTarget:         (Option[Target.Id], SetRouteVia) => Callback,
//   onCloneTarget:     OnCloneParameters => Callback,
//   onAsterismUpdate:  OnAsterismUpdateParams => Callback,
//   obsInfo:           Target.Id => TargetEditObsInfo,
//   searching:         View[Set[Target.Id]],
//   title:             String,
//   globalPreferences: View[GlobalPreferences],
//   readonly:          Boolean,
//   sequenceChanged:   Callback = Callback.empty,
//   backButton:        Option[VdomNode] = none
// )(using FetchClient[IO, ObservationDB], Logger[IO]): Tile = {
//   // Save the time here. this works for the obs and target tabs
//   // It's OK to save the viz time for executed observations, I think.
//   val vizTimeView =
//     vizTime.withOnMod(t => ObsQueries.updateVisualizationTime[IO](obsIds.toList, t).runAsync)
//
//   val control: VdomNode =
//     <.div(ExploreStyles.JustifiedEndTileControl, ObsTimeEditor(vizTimeView))
//
//   Tile(
//     ObsTabTilesIds.TargetId.id,
//     title,
//     back = backButton,
//     canMinimize = true,
//     control = s => control.some.filter(_ => s === TileSizeState.Minimized),
//     bodyClass = ExploreStyles.TargetTileBody,
//     controllerClass = ExploreStyles.TargetTileController
//   )((renderInTitle: Tile.RenderInTitle) =>
//     userId.map(uid =>
//       AsterismEditor(
//         uid,
//         programId,
//         obsIds,
//         obsAndTargets,
//         vizTime,
//         obsConf,
//         currentTarget,
//         setTarget,
//         onCloneTarget,
//         onAsterismUpdate,
//         obsInfo,
//         searching,
//         renderInTitle,
//         globalPreferences,
//         readonly,
//         sequenceChanged
//       )
//     )
//   )
// }
