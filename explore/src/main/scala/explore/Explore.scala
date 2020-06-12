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
import gem.Observation
import gem.ProgramId
import gsp.math.Index

@JSExportTopLevel("Explore")
object ExploreMain extends AppMain {

  protected val (router, routerCtl) =
    RouterWithProps.componentAndCtl(BaseUrl.fromWindowOrigin, Routing.config)

  private val obsId = Observation
    .Id(ProgramId.Science.fromString.getOption("GS-2020A-DS-1").get, Index.One)

  protected def routingView(view: View[RootModel]): View[RootModel] =
    ViewF[IO, RootModel](
      view.get,
      f =>
        view.mod { model =>
          val newModel = f(model)
          // Having this here makes sure the URL update is always executed.
          // Another option is to move it to AppRoot.componentDidUpdate, but it won't be
          // executed if the application isn't rerendered because of reusability.
          routerCtl
            .set( // Can we make this a silent set? We don't want to trigger the router here.
              RootModelRouting.lens.get(newModel.copy(obsId = obsId.some))
            )
            .runNow()
          newModel
        }
    )

  override def rootComponent(view: View[RootModel]): VdomElement =
    <.div(
      router(routingView(view))
    )

}
