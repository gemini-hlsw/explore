// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.common.ObsQueries._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.constraints.ConstraintsPanel
import explore.implicits._
import explore.model.reusability._
import explore.undo._
import explore.utils._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.ui.reusability._
import react.common._

object ConstraintsTile {

  def constraintsTile(
    obsId:      Observation.Id,
    csPot:      Pot[View[ConstraintSetData]],
    undoStacks: View[UndoStacks[IO, ConstraintSetData]]
  ): Tile =
    Tile(
      ObsTabTiles.ConstraintsId,
      "Constraints",
      canMinimize = true
    )(
      (csPot, undoStacks).curryReusing.in((csPotView_, undoStacks_, renderInTitle) =>
        potRender[View[ConstraintSetData]](
          Reuse.always(cs =>
            <.div(
              ExploreStyles.ConstraintsObsTile,
              ConstraintsPanel(obsId, cs, undoStacks_, renderInTitle)
            )
          )
        )(csPotView_)
      )
    )

}
