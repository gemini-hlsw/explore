// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import java.util.UUID

import cats.syntax.all._
import crystal.react.implicits._
import explore.model.Page
import explore.model.Page._
import explore.model._
import explore.proposal._
import explore.tabs._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.VdomElement
import monocle.Iso

sealed trait ElementItem  extends Product with Serializable
case object IconsElement  extends ElementItem
case object LabelsElement extends ElementItem

object Routing {

  private val obsPageIso: Iso[ExploreObservation.Id, ObsPage] =
    Iso[ExploreObservation.Id, ObsPage](ObsPage.apply)(_.obsId)

  private val targetPageIso: Iso[SiderealTarget.Id, TargetPage] =
    Iso[SiderealTarget.Id, TargetPage](TargetPage.apply)(_.targetId)

  private val targetObsPageIso: Iso[ExploreObservation.Id, TargetsObsPage] =
    Iso[ExploreObservation.Id, TargetsObsPage](TargetsObsPage.apply)(_.obsId)

  val config: RouterWithPropsConfig[Page, View[RootModel]] =
    RouterWithPropsConfigDsl[Page, View[RootModel]].buildConfig { dsl =>
      import dsl._

      (emptyRule
        | staticRoute(root, HomePage) ~> render(UnderConstruction())
        | staticRoute("/proposal", ProposalPage) ~> renderP(view =>
          ProposalTabContents(view.zoom(RootModel.focused))
        )
        | staticRoute("/observations", ObservationsBasePage) ~> renderP(view =>
          ObsTabContents(view.zoom(RootModel.focused))
        )
        | dynamicRouteCT(("/obs" / uuid).xmapL(obsPageIso)) ~> renderP(view =>
          ObsTabContents(view.zoom(RootModel.focused))
        )
        | staticRoute("/targets", TargetsBasePage) ~> renderP(view =>
          TargetTabContents(view.zoom(RootModel.focused))
        )
        | dynamicRouteCT(("/target" / uuid).xmapL(targetPageIso)) ~> renderP(view =>
          TargetTabContents(view.zoom(RootModel.focused))
        )
        | dynamicRouteCT(("/target/obs" / uuid).xmapL(targetObsPageIso)) ~> renderP(view =>
          TargetTabContents(view.zoom(RootModel.focused))
        )
        | staticRoute("/configurations", ConfigurationsPage) ~> render(UnderConstruction())
        | staticRoute("/constraints", ConstraintsPage) ~> render(UnderConstruction()))
        .notFound(redirectToPage(HomePage)(SetRouteVia.HistoryPush))
        .verify(
          HomePage,
          ProposalPage,
          ObservationsBasePage,
          ObsPage(UUID.randomUUID),
          TargetsBasePage,
          TargetPage(UUID.randomUUID),
          TargetsObsPage(UUID.randomUUID),
          ConfigurationsPage,
          ConstraintsPage
        )
        .onPostRenderP {
          case (prev, next, view)
              if next.some =!= prev &&
                // Short circuit if we get here because of a change in the model.
                next =!= view.zoom(RootModelRouting.lens).get =>
            view.zoom(RootModelRouting.lens).set(next).runInCB
          case _ => Callback.empty
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
