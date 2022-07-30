// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import crystal.react.View
import eu.timepit.refined.auto._
import explore._
import explore.components.Tile
import explore.constraints.ConstraintsPanel
import explore.undo._
import explore.utils._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.style.Css

object ConstraintsTile {

  def constraintsTile(
    obsId:        Observation.Id,
    csPot:        Pot[View[ConstraintSet]],
    undoStacks:   View[UndoStacks[IO, ConstraintSet]],
    control:      Option[VdomNode] = None,
    clazz:        Option[Css] = None
  )(implicit ctx: AppContextIO): Tile =
    Tile(
      ObsTabTilesIds.ConstraintsId,
      "Constraints",
      canMinimize = true,
      control = _ => control,
      controllerClass = clazz
    )(renderInTitle =>
      potRender[View[ConstraintSet]](cs =>
        ConstraintsPanel(List(obsId), cs, undoStacks, renderInTitle)
      )(csPot)
    )

}
