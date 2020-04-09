// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.model._
import japgolly.scalajs.react.extra.router._
import gem.Observation
import japgolly.scalajs.react.MonocleReact._
import monocle.Prism
import japgolly.scalajs.react.Callback
import crystal.react.implicits._

sealed trait ElementItem extends Product with Serializable
case object IconsElement extends ElementItem
case object LabelsElement extends ElementItem

sealed trait Page extends Product with Serializable
case object HomePage extends Page
final case class ObsPage(obsId: Observation.Id) extends Page

class Routing(viewCtx: ViewCtxIO[RootModel]) {

  private val obsIdP: Prism[String, ObsPage] =
    Prism[String, ObsPage] {
      case s =>
        println(s)
        println(Observation.Id.fromString(s))
        Observation.Id.fromString(s).map(ObsPage(_))
    }(p => p.obsId.format)

  val config: RouterConfig[Page] = RouterConfigDsl[Page].buildConfig { dsl =>
    import dsl._

    (emptyRule
      | staticRoute(root, HomePage) ~> render(HomeComponent(viewCtx))
      | dynamicRouteCT((("/obs" / string("[a-zA-Z0-9-]+"))).pmapL(obsIdP)) ~> render(
        HomeComponent(viewCtx)
      ))
      .notFound(redirectToPage(HomePage)(SetRouteVia.HistoryPush))
      .verify(HomePage, ObsPage(Observation.Id.unsafeFromString("GS2020A-Q-1")))
      .onPostRender {
        case (_, ObsPage(id)) =>
          viewCtx.view.zoomL(RootModel.id).set(Option(id)).toCB *>
            Callback.log(s"id:1 $id")
        case _ => Callback.empty
      }
      .renderWith(layout)
      .logToConsole
  }

  private def layout(c: RouterCtl[Page], r: Resolution[Page]) =
    OTLayout(c, r)(viewCtx.get)
}
