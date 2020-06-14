// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.implicits._
import crystal.react.implicits._
import explore.model._
import explore.model.Page
import explore.model.Page._
import gem.Observation
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.VdomElement
import monocle.Prism

sealed trait ElementItem  extends Product with Serializable
case object IconsElement  extends ElementItem
case object LabelsElement extends ElementItem

object Routing {

  private val obsIdP: Prism[String, ObsPage] =
    Prism[String, ObsPage] {
      case s =>
        Observation.Id.fromString(s).map(ObsPage(_))
    }(p => p.obsId.format)

  val config: RouterWithPropsConfig[Page, View[RootModel]] =
    RouterWithPropsConfigDsl[Page, View[RootModel]].buildConfig { dsl =>
      import dsl._

      (emptyRule
        | staticRoute(root, HomePage) ~> renderP(view => HomeComponent(view))
        | staticRoute("/constraints", ConstraintsPage) ~> render(UnderConstruction())
        | dynamicRouteCT(("/obs" / string("[a-zA-Z0-9-]+")).pmapL(obsIdP)) ~> renderP(view =>
          HomeComponent(view)
        ))
        .notFound(redirectToPage(HomePage)(SetRouteVia.HistoryPush))
        .verify(HomePage, ObsPage(Observation.Id.unsafeFromString("GS2020A-Q-1")))
        .onPostRenderP {
          case (prev, next, view) if prev =!= next.some =>
            Callback.log(s"Routing.onPostRender triggered [$prev] => [$next]") >>
              view.zoomL(RootModelRouting.lens).set(next).runInCB
          case _                                        => Callback.empty
        }
        .renderWithP(layout)
    // .logToConsole
    }

  private def layout(
    c: RouterCtl[Page],
    r: ResolutionWithProps[Page, View[RootModel]]
  ): View[RootModel] => VdomElement =
    OTLayout(c, r)
}
