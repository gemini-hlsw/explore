// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import scala.scalajs.js.annotation.JSExportTopLevel

import explore.AppMain
import explore._
import explore.model.RootModel
import japgolly.scalajs.react.vdom.html_<^._

import TargetObsQueries._

@JSExportTopLevel("ObsTreeTest")
object Test extends AppMain {

  override def rootComponent(view: View[RootModel]): VdomElement =
    // AndOrTest.render
    // TargetTree(TargetTreeTest.targets, TargetTreeTest.observations)
    // TargetObsList(TargetTreeTest.targets, ViewF(obs.get.unsafeRunSync(), obs.update))
    TargetObsSubscription(targetsWithObs =>
      <.div(^.width := "295px")(
        TargetObsList(
          targetsWithObs,
          view.zoom(RootModel.focused)
        )
      )
    )
}
