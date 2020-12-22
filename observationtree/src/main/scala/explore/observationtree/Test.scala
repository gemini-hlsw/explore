// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import explore.components.state.IfLogged
import explore.model.RootModel
import explore.{ AppMain, _ }
import japgolly.scalajs.react.vdom.html_<^._

import scala.scalajs.js.annotation.JSExportTopLevel

import TargetObsQueries._

@JSExportTopLevel("ObsTreeTest")
object Test extends AppMain {

  override def rootComponent(view: View[RootModel]): VdomElement =
    IfLogged(view)((_, _) =>
      // AndOrTest.render
      // TargetTree(TargetTreeTest.targets, TargetTreeTest.observations)
      TargetObsLiveQuery(targetsWithObs =>
        <.div(^.width := "295px")(
          TargetObsList(
            targetsWithObs,
            view.zoom(RootModel.focused),
            view.zoom(RootModel.targetViewExpandedIds)
          )
        )
      )
    )
}
