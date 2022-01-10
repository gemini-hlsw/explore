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
import explore.model.ObsIdSet
import explore.model.TargetVisualOptions
import explore.model.TargetWithId
import explore.model.reusability._
import explore.targeteditor.AsterismEditor
import explore.undo.UndoStacks
import explore.utils._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.SiderealTarget
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import react.common._

object TargetTile {

  def targetTile(
    userId:        Option[User.Id],
    obsId:         Observation.Id,
    asterismPot:   Pot[View[List[TargetWithId]]],
    undoStacks:    View[Map[Target.Id, UndoStacks[IO, SiderealTarget]]],
    searching:     View[Set[Target.Id]],
    options:       View[TargetVisualOptions],
    hiddenColumns: View[Set[String]]
  )(implicit ctx:  AppContextIO) =
    Tile(ObsTabTiles.TargetId, "Targets", canMinimize = true)(
      Reuse.by((userId, obsId, asterismPot, undoStacks, searching, options))(
        (renderInTitle: Tile.RenderInTitle) =>
          potRender[View[List[TargetWithId]]](
            (
              (asterism: View[List[TargetWithId]]) =>
                userId.map(uid =>
                  <.div(
                    AsterismEditor(uid,
                                   ObsIdSet.one(obsId),
                                   asterism,
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
            asterismPot
          )
      )
    )

}
