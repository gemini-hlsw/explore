// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.components.Tile
import explore.constraints.ConstraintsPanel
import explore.model.ConstraintSet
import explore.model.reusability._
import explore.undo._
import explore.utils._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.ui.reusability._
import react.common._
import react.common.style.Css

object ConstraintsTile {

  def constraintsTile(
    obsId:      Observation.Id,
    csPot:      Pot[View[ConstraintSet]],
    undoStacks: View[UndoStacks[IO, ConstraintSet]],
    control:    Option[Reuse[VdomNode]] = None,
    clazz:      Option[Css] = None
  ): Tile =
    Tile(
      ObsTabTiles.ConstraintsId,
      "Constraints",
      canMinimize = true,
      control = control,
      controllerClass = clazz
    )(
      (csPot, undoStacks).curryReusing.in((csPotView_, undoStacks_, renderInTitle) =>
        potRender[View[ConstraintSet]](
          Reuse.always(cs =>
            <.div(
              ConstraintsPanel(List(obsId), cs, undoStacks_, renderInTitle)
            )
          )
        )(csPotView_)
      )
    )

}
