// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import crystal.react.implicits._
import explore.model._
import gem.Observation
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.router._
import monocle.Prism
import japgolly.scalajs.react.vdom.VdomElement

sealed trait ElementItem  extends Product with Serializable
case object IconsElement  extends ElementItem
case object LabelsElement extends ElementItem

sealed trait Page    extends Product with Serializable
case object HomePage extends Page
final case class ObsPage(obsId: Observation.Id) extends Page

object Routing {

  private val obsIdP: Prism[String, ObsPage] =
    Prism[String, ObsPage] {
      case s =>
        println(s)
        println(Observation.Id.fromString(s))
        Observation.Id.fromString(s).map(ObsPage(_))
    }(p => p.obsId.format)

  val config: RouterWithPropsConfig[Page, ViewCtxIO[RootModel]] =
    RouterWithPropsConfigDsl[Page, ViewCtxIO[RootModel]].buildConfig { dsl =>
      import dsl._

      (emptyRule
        | staticRoute(root, HomePage) ~> renderP(viewCtx => HomeComponent(viewCtx))
        | dynamicRouteCT(("/obs" / string("[a-zA-Z0-9-]+")).pmapL(obsIdP)) ~> renderP(viewCtx =>
          HomeComponent(viewCtx)
        ))
        .notFound(redirectToPage(HomePage)(SetRouteVia.HistoryPush))
        .verify(HomePage, ObsPage(Observation.Id.unsafeFromString("GS2020A-Q-1")))
        .onPostRenderP {
          case (_, ObsPage(id), viewCtx) =>
            viewCtx.zoomL(RootModel.id).set(Option(id)).runInCB *>
              Callback.log(s"id:1 $id")
          case _                         => Callback.empty
        }
        .renderWithP(layout)
        .logToConsole
    }

  private def layout(
    c: RouterCtl[Page],
    r: ResolutionWithProps[Page, ViewCtxIO[RootModel]]
  ): ViewCtxIO[RootModel] => VdomElement =
    viewCtx => OTLayout(c, r)(viewCtx)
}
