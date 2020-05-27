// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.scalajs.js

import explore.Routing
import explore.model.RootModel
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

import js.annotation._

@JSExportTopLevel("Explore")
object ExploreMain extends AppMain {

  private val router = RouterWithProps(BaseUrl.fromWindowOrigin, Routing.config)

  override def rootComponent(view: View[RootModel]): VdomElement =
    <.div(
      router(view)
    )

}
