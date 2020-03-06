// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect._
import crystal._
import explore.model._
import japgolly.scalajs.react.extra.router._

sealed trait ElementItem extends Product with Serializable
case object IconsElement extends ElementItem
case object LabelsElement extends ElementItem

sealed trait Page extends Product with Serializable
case object HomePage extends Page
final case class ElementPage(e: ElementItem) extends Page

class Routing(modelView: View[IO, RootModel]) {
  val WithModel = modelView.streamRender
  // import AppStateIO._
  // val WithModel = react.StreamRenderer.build(modelView.stream.debug())

  val config: RouterConfig[Page] = RouterConfigDsl[Page].buildConfig { dsl =>
    import dsl._

    (
      staticRoute(root, HomePage) ~>
        render(WithModel(HomeComponentGQL.component(_)))
    ).notFound(redirectToPage(HomePage)(SetRouteVia.HistoryPush))
      .renderWith(layout)
      .logToConsole
  }

  private def layout(c: RouterCtl[Page], r: Resolution[Page]) = WithModel(OTLayoutGQL(c, r)(_))
}
