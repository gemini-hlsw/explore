// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.implicits._
import cats.effect._
import explore.model._
import japgolly.scalajs.react.extra.router._
import crystal.react.AppRoot
import cats.kernel.Monoid

sealed trait ElementItem extends Product with Serializable
case object IconsElement extends ElementItem
case object LabelsElement extends ElementItem

sealed trait Page extends Product with Serializable
case object HomePage extends Page
final case class ElementPage(e: ElementItem) extends Page

class Routing(initialModel: RootModel)(
  implicit ctx:             AppContext[IO],
  ce:                       ConcurrentEffect[IO],
  timer:                    Timer[IO],
  monoidf:                  Monoid[IO[Unit]]
) {
  val WithModelCtx = AppRoot.component[IO](initialModel, ctx)(ctx.cleanup.some)

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
