// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.implicits._
import crystal.react.implicits._
import explore.model.Page
import explore.model.Page._
import explore.model._
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

  private val targetIdP: Prism[String, TargetPage] =
    Prism[String, TargetPage] {
      case s =>
        SiderealTarget.Id.fromString(s).map(TargetPage(_))
    }(p => p.targetId.format)

  private val targetObsIdP: Prism[String, TargetsObsPage] =
    Prism[String, TargetsObsPage] {
      case s =>
        Observation.Id.fromString(s).map(TargetsObsPage(_))
    }(p => p.obsId.format)

  val config: RouterWithPropsConfig[Page, View[RootModel]] =
    RouterWithPropsConfigDsl[Page, View[RootModel]].buildConfig { dsl =>
      import dsl._

      (emptyRule
        | staticRoute(root, HomePage) ~> renderP(view => HomeComponent(view))
        | dynamicRouteCT(("/obs" / string("[a-zA-Z0-9-]+")).pmapL(obsIdP)) ~> renderP(view =>
          HomeComponent(view)
        )
        | dynamicRouteCT(("/target" / string("[a-zA-Z0-9-\\s]+")).pmapL(targetIdP)) ~> renderP(
          view => HomeComponent(view)
        )
        | dynamicRouteCT(("/target/obs" / string("[a-zA-Z0-9-]+")).pmapL(targetObsIdP)) ~> renderP(
          view => HomeComponent(view)
        )
        | staticRoute("/configurations", ConfigurationsPage) ~> render(UnderConstruction())
        | staticRoute("/constraints", ConstraintsPage) ~> render(UnderConstruction()))
        .notFound(redirectToPage(HomePage)(SetRouteVia.HistoryPush))
        .verify(
          HomePage,
          ObsPage(Observation.Id.unsafeFromString("GS2020A-Q-1")),
          TargetPage(SiderealTarget.Id.unsafeFromString("NGC 891")),
          TargetsObsPage(Observation.Id.unsafeFromString("GS2020A-Q-1")),
          ConfigurationsPage,
          ConstraintsPage
        )
        .onPostRenderP {
          case (_, next, view) if next =!= RootModelRouting.lens.get(view.get) =>
            Callback.log(
              s"Routing.onPostRender triggered [${RootModelRouting.lens.get(view.get)}] => [$next]"
            ) >>
              view.zoomL(RootModelRouting.lens).set(next).runInCB
          case _                                                               => Callback.empty
        }
        .renderWithP(layout)
        .logToConsole
    }

  private def layout(
    c: RouterCtl[Page],
    r: ResolutionWithProps[Page, View[RootModel]]
  ): View[RootModel] => VdomElement =
    OTLayout(c, r)
}
