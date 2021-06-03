// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.common.TargetQueriesGQL._
import explore.components.Tile
import explore.components.graphql.LiveQueryRenderMod
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model._
import explore.schemas.ObservationDB
import explore.targeteditor.TargetBody
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._

object TargetTile {
  def targetTile(
    userId:            Option[User.Id],
    targetId:          Option[Target.Id],
    searching:         View[Set[Target.Id]],
    targetViewOptions: View[TargetVisualOptions]
  )(implicit ctx:      AppContextIO) = {
    def targetRenderFn(
      targetId:      Target.Id,
      renderInTitle: Tile.RenderInTitle,
      targetOpt:     View[Option[TargetEditQuery.Data.Target]]
    ): VdomNode =
      (userId, targetOpt.get).mapN { case (uid, _) =>
        TargetBody(uid,
                   targetId,
                   targetOpt.zoom(_.get)(f => _.map(f)),
                   searching,
                   targetViewOptions,
                   renderInTitle
        )
      }

    def renderTarget(
      targetId:      Option[Target.Id],
      renderInTitle: Tile.RenderInTitle
    ): VdomNode =
      targetId
        .map[VdomNode] { targetId =>
          LiveQueryRenderMod[ObservationDB,
                             TargetEditQuery.Data,
                             Option[TargetEditQuery.Data.Target]
          ](
            TargetEditQuery.query(targetId),
            _.target,
            List(TargetEditSubscription.subscribe[IO](targetId))
          )(Reuse(targetRenderFn _)(targetId, renderInTitle))
            .withKey(s"target-$targetId")
        }
        .getOrElse(
          <.div(ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
                <.div("No target assigned")
          )
        )

    Tile(ObsTabTiles.TargetId, "Target", canMinimize = true)(Reuse(renderTarget _)(targetId))
  }

}
