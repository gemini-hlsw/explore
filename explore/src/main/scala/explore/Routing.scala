// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.implicits._
import cats.effect._
import explore.model._
import japgolly.scalajs.react.extra.router._
import crystal.react.AppRoot

sealed trait ElementItem extends Product with Serializable
case object IconsElement extends ElementItem
case object LabelsElement extends ElementItem

sealed trait Page extends Product with Serializable
case object HomePage extends Page
final case class ElementPage(e: ElementItem) extends Page

class Routing(initialModel: RootModel)(
  implicit ctx:             AppContext[Eff],
  ce:                       ConcurrentEffect[Eff],
  timer:                    Timer[Eff]
) {
  val WithModelCtx = AppRoot.component[Eff](initialModel, ctx)

  val config: RouterConfig[Page] = RouterConfigDsl[Page].buildConfig { dsl =>
    import dsl._

    (
      staticRoute(root, HomePage) ~>
        render(WithModelCtx(HomeComponent(_)))
    ).notFound(redirectToPage(HomePage)(SetRouteVia.HistoryPush))
      .renderWith(layout)
      .logToConsole
  }

  private def layout(c: RouterCtl[Page], r: Resolution[Page]) =
    WithModelCtx(viewCtx => OTLayout(c, r)(viewCtx.get))
}
