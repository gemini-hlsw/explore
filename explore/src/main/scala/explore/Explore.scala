// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import crystal.react.implicits._
import explore.Routing
import explore.model.Focused
import explore.model.RootModel
import explore.model.RootModelRouting
import explore.model.enum.AppTab
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

import scala.scalajs.js

import js.annotation._

@JSExportTopLevel("Explore")
object ExploreMain extends AppMain {

  protected val (router, routerCtl) =
    RouterWithProps.componentAndCtl(BaseUrl.fromWindowOrigin, Routing.config)

  protected def routingView(view: View[RootModel]): View[RootModel] =
    view.withOnMod { model =>
      routerCtl.set(RootModelRouting.lens.get(model)).to[IO]
    }

  override def rootComponent(view: View[RootModel]): VdomElement =
    <.div(
      router(routingView(view))
    )

  override protected def pageUrl(tab: AppTab, focused: Option[Focused]): String =
    routerCtl.urlFor(RootModelRouting.getPage(tab, focused)).value
}
