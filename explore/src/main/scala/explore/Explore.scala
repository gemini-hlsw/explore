// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.scalajs.js

import cats.effect.IO
import cats.implicits._
import crystal.ViewF
import crystal.react.implicits._
import explore.Routing
import explore.model.Page
import explore.model.RootModel
import explore.model.RootModelRouting
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

import js.annotation._

@JSExportTopLevel("Explore")
object ExploreMain extends AppMain {

  protected val (router, routerCtl) =
    RouterWithProps.componentAndCtl(BaseUrl.fromWindowOrigin, Routing.config)

  protected def routingView(view: View[RootModel]): View[RootModel] =
    ViewF[IO, RootModel](
      view.get,
      f =>
        view.mod { model =>
          val newModel = f(model)
          // Having this here makes sure the URL update is always executed.
          // Another option is to move it to AppRoot.componentDidUpdate, but it won't be
          // executed if the application isn't rerendered because of reusability.
          routerCtl.set(RootModelRouting.lens.get(newModel)).runNow()
          newModel
        }
    )

  override def rootComponent(view: View[RootModel]): VdomElement =
    <.div(
      router(routingView(view))
    )

}
