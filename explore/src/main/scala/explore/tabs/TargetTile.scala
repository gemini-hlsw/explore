// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import crystal.react.implicits._
import crystal.react.reuse._
import explore.components.Tile
import explore.implicits._
import explore.model.ScienceTarget
import explore.model.TargetEnv
import explore.model.TargetVisualOptions
import explore.model.reusability._
import explore.targeteditor.TargetEnvEditor
import explore.undo.UndoStacks
import explore.utils._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.SiderealTarget
import lucuma.core.model.User
import lucuma.ui.reusability._
import react.common._

object TargetTile {

  // protected def renderFn(
  //   uid:           User.Id,
  //   targetId:      Target.Id,
  //   target:        View[SiderealTarget],
  //   undoStacks:    View[UndoStacks[IO, SiderealTarget]],
  //   searching:     View[Set[Target.Id]],
  //   options:       View[TargetVisualOptions],
  //   renderInTitle: Tile.RenderInTitle
  // ): VdomNode =
  //   SiderealTarget(uid, targetId, target, undoStacks, searching, options, renderInTitle)

  def targetTile(
    userId:        Option[User.Id],
    targetEnvPot:  Pot[View[TargetEnv]],
    undoStacks:    View[Map[ScienceTarget.Id, UndoStacks[IO, SiderealTarget]]],
    searching:     View[Set[ScienceTarget.Id]],
    options:       View[TargetVisualOptions],
    hiddenColumns: View[Set[String]]
  ) = //(implicit ctx: AppContextIO) =
    Tile(ObsTabTiles.TargetId, "Targets", canMinimize = true)(
      Reuse.by((userId, targetEnvPot, undoStacks, searching, options))(
        (renderInTitle: Tile.RenderInTitle) =>
          potRender[View[TargetEnv]](
            (
              (targetEnv: View[TargetEnv]) =>
                userId.map(uid =>
                  <.div(
                    TargetEnvEditor(uid,
                                    targetEnv,
                                    undoStacks,
                                    searching,
                                    options,
                                    hiddenColumns,
                                    renderInTitle
                    )
                    // SiderealTargetEditor(
                    //   uid,
                    //   targetEnv.get.scienceTargets.head.id,
                    //   targetEnv
                    //     .zoom(TargetEnv.scienceTargets)
                    //     .zoom(_.head.target.asInstanceOf[SiderealTarget])(mod =>
                    //       list =>
                    //         ScienceTarget(list.head.id,
                    //                       mod(list.head.target.asInstanceOf[SiderealTarget])
                    //         ) +: list.tail
                    //     ),
                    //   undoStacks
                    //     .zoom(
                    //       atMapWithDefault(targetEnv.get.scienceTargets.head.id, UndoStacks.empty)
                    //     ),
                    //   searching,
                    //   options,
                    //   renderInTitle
                    // )
                  )
                ): VdomNode
            ).reuseAlways
          )(
            targetEnvPot
          )
      )
      // (
      //   (renderFn _).reuseCurrying(
      //     userId,
      //     targetId,
      //     tePot.zoom(???),
      //     undoStacks.zoom(???),
      //     searching,
      //     options
      //   )
      // )
    )

}

// def targetTile(
//   userId:       Option[User.Id],
//   targetEnvId:  TargetEnvironment.Id
// )(implicit ctx: AppContextIO) =
//   Tile(ObsTabTiles.TargetId, "Targets", canMinimize = true)(
//     (
//       (_: Tile.RenderInTitle) =>
//         LiveQueryRenderMod[ObservationDB,
//                            TargetEnvQuery.Data,
//                            Option[TargetEnvQuery.Data.TargetEnvironment]
//         ](
//           TargetEnvQuery.query(targetEnvId).reuseAlways,
//           (TargetEnvQuery.Data.targetEnvironment.get _).reuseAlways,
//           List(TargetEnvEditSubscription.subscribe[IO](targetEnvId)).reuseAlways
//         )(
//           potRender(
//             (renderFn _).reuseAlways
//           )
//         )
//           .withKey(s"targetEnv-$targetEnvId"): VdomNode
//     ).reuseAlways
// )

// def targetTile(
//   userId:            Option[User.Id],
//   targetId:          Option[Target.Id],
//   undoStacks:        View[Map[Target.Id, UndoStacks[IO, TargetResult]]],
//   searching:         View[Set[Target.Id]],
//   targetViewOptions: View[TargetVisualOptions]
// )(implicit ctx:      AppContextIO) = {

//   def targetRenderFn(
//     targetId:      Target.Id,
//     undoStacks:    View[UndoStacks[IO, TargetResult]],
//     renderInTitle: Tile.RenderInTitle,
//     targetOpt:     View[Option[TargetEditQuery.Data.Target]]
//   ): VdomNode =
//     (userId, targetOpt.get).mapN { case (uid, _) =>
//       TargetBody(
//         uid,
//         targetId,
//         targetOpt.zoom(_.get)(f => _.map(f)),
//         undoStacks,
//         searching,
//         targetViewOptions,
//         renderInTitle
//       )
//     }

//   def renderTarget(
//     targetIdUndoStacks: Option[(Target.Id, View[UndoStacks[IO, TargetResult]])],
//     renderInTitle:      Tile.RenderInTitle
//   ): VdomNode =
//     targetIdUndoStacks
//       .map[VdomNode] { case (targetId, undoStacks) =>
//         LiveQueryRenderMod[ObservationDB,
//                            TargetEditQuery.Data,
//                            Option[TargetEditQuery.Data.Target]
//         ](
//           TargetEditQuery.query(targetId).reuseAlways,
//           (TargetEditQuery.Data.target.get _).reuseAlways,
//           List(TargetEditSubscription.subscribe[IO](targetId)).reuseAlways
//         )(
//           potRender(
//             Reuse(targetRenderFn _)(targetId, undoStacks, renderInTitle)
//           )
//         )
//           .withKey(s"target-$targetId")
//       }
//       .getOrElse(
//         <.div(ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
//               <.div("No target assigned")
//         )
//       )

//   Tile(ObsTabTiles.TargetId, "Target", canMinimize = true)(
//     targetId
//       .map(tid =>
//         (tid, undoStacks.zoom(atMapWithDefault(tid, UndoStacks.empty[IO, TargetResult])))
//       )
//       .curryReusing
//       .in(renderTarget _)
//   )
// }

// }
