// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.conditions

import cats.effect.IO
import scala.scalajs.js
import js.annotation._
import gem.Observation
import gem.ProgramId
import gsp.math.Index
import crystal.react.AppRoot
import explore.model.RootModel
import explore.AppMain
import japgolly.scalajs.react.vdom.VdomElement

@JSExportTopLevel("Test")
object Test extends AppMain {

  override def rootComponent(
    WithModelCtx: AppRoot.Component[IO, explore.AppContextIO, RootModel]
  ): VdomElement =
    WithModelCtx { viewCtx =>
      implicit val ctx = viewCtx.ctx

      ConditionsPanel(
        Observation.Id(ProgramId.Science.fromString.getOption("GS-2020A-DS-1").get, Index.One)
      )
    }

}
