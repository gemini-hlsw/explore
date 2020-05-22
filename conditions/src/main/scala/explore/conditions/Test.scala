// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.conditions

import scala.scalajs.js

import crystal.implicits._
import explore.AppMain
import explore._
import explore.model.RootModel
import gem.Observation
import gem.ProgramId
import gsp.math.Index
import japgolly.scalajs.react.vdom.VdomElement

import js.annotation._

@JSExportTopLevel("Test")
object Test extends AppMain {

  override def rootComponent(view: View[RootModel]): VdomElement =
    ConditionsPanel(
      Observation
        .Id(ProgramId.Science.fromString.getOption("GS-2020A-DS-1").get, Index.One)
    )

}
