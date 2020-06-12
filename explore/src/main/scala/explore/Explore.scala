// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.scalajs.js

import cats.implicits._
import crystal.react.implicits._
import explore.Routing
import explore.model.RootModel
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

import js.annotation._
import crystal.ViewF
import cats.effect.IO
import explore.model.RootModelRouting
import explore.model.Page

@JSExportTopLevel("Explore")
object ExploreMain extends AppMain {

  protected val (router, routerLogic) =
    RouterWithProps.componentAndLogic(BaseUrl.fromWindowOrigin, Routing.config)

  // This builds a RouterCtl that doesn't Broadcast when a new Page is set.
  // Maybe we can send a PR to the RouterCtl to add a silent: Boolean parameter to set?
  protected val routerSilentCtl: RouterCtl[Page] = {
    val routerCtlByPath = routerLogic.ctlByPath

    @inline implicit def impbaseurl: BaseUrl = routerLogic.baseUrl

    new RouterCtl[Path] {
      override def baseUrl = routerCtlByPath.baseUrl
      override def byPath  = this
      override val refresh = routerCtlByPath.refresh
      override def pathFor(path: Path) = routerCtlByPath.pathFor(path)
      override def set(p:        Path, v: SetRouteVia) =
        routerLogic.interpret(RouteCmd.setRoute(p.abs, v))
    }.contramap(Routing.config.rules.path)
  }

  protected def routingView(view: View[RootModel]): View[RootModel] =
    ViewF[IO, RootModel](
      view.get,
      f =>
        view.mod { model =>
          val newModel = f(model)
          // Having this here makes sure the URL update is always executed.
          // Another option is to move it to AppRoot.componentDidUpdate, but it won't be
          // executed if the application isn't rerendered because of reusability.
          routerSilentCtl.set(RootModelRouting.lens.get(newModel)).runNow()
          newModel
        }
    )

  override def rootComponent(view: View[RootModel]): VdomElement =
    <.div(
      router(routingView(view))
    )

}
