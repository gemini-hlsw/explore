package explore

import japgolly.scalajs.react.extra.router._
import explore.model.Page
import explore.model.RootModel
import explore.model.Page.HomePage

trait DummyRouting {
  protected val config: RouterConfig[Page] =
    RouterConfigDsl[Page].buildConfig { dsl =>
      import dsl._

      emptyRule
        .notFound(redirectToPage(HomePage)(SetRouteVia.HistoryPush))
    }

  protected val routerCtl: RouterCtl[Page] =
    new RouterLogic(BaseUrl.fromWindowOrigin, config).ctl
}
