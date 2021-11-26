// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse._
import explore.components.Tile
import explore.implicits._
import explore.model.TargetEnvGroup
import explore.model.TargetIdSet
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

  def targetTile(
    userId:        Option[User.Id],
    targetEnvPot:  Pot[View[TargetEnvGroup]],
    undoStacks:    View[Map[TargetIdSet, UndoStacks[IO, SiderealTarget]]],
    searching:     View[Set[TargetIdSet]],
    options:       View[TargetVisualOptions],
    hiddenColumns: View[Set[String]]
  )(implicit ctx:  AppContextIO) =
    Tile(ObsTabTiles.TargetId, "Targets", canMinimize = true)(
      Reuse.by((userId, targetEnvPot, undoStacks, searching, options))(
        (renderInTitle: Tile.RenderInTitle) =>
          potRender[View[TargetEnvGroup]](
            (
              (targetEnv: View[TargetEnvGroup]) =>
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
                  )
                ): VdomNode
            ).reuseAlways
          )(
            targetEnvPot
          )
      )
    )

}
