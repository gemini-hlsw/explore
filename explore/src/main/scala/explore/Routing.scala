// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.util.Random

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
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.util.Gid

sealed trait ElementItem  extends Product with Serializable
case object IconsElement  extends ElementItem
case object LabelsElement extends ElementItem

object Routing {

  private def randomId[Id](fromLong: Long => Option[Id]): Id =
    fromLong(Random.nextLong().abs).get

  private def targetTab(model: View[RootModel]): TargetTabContents =
    TargetTabContents(model.zoom(RootModel.focused), model.zoom(RootModel.expandedTargetIds))

  val config: RouterWithPropsConfig[Page, View[RootModel]] =
    RouterWithPropsConfigDsl[Page, View[RootModel]].buildConfig { dsl =>
      import dsl._

      def id[Id](implicit gid: Gid[Id]): StaticDsl.RouteB[Id] =
        string(gid.regexPattern).pmapL(gid.fromString)

      (emptyRule
        | staticRoute(root, HomePage) ~> render(UnderConstruction())
        | staticRoute("/proposal", ProposalPage) ~> renderP(view =>
          ProposalTabContents(view.zoom(RootModel.focused))
        )
        | staticRoute("/observations", ObservationsBasePage) ~> renderP(view =>
          ObsTabContents(view.zoom(RootModel.focused))
        )
        | dynamicRouteCT(("/obs" / id[Observation.Id]).xmapL(ObsPage.obsId)) ~> renderP(view =>
          ObsTabContents(view.zoom(RootModel.focused))
        )
        | staticRoute("/targets", TargetsBasePage) ~> renderP(targetTab)
        | dynamicRouteCT(("/target" / id[Target.Id]).xmapL(TargetPage.targetId)) ~> renderP(
          targetTab
        )
        | dynamicRouteCT(
          ("/target/obs" / id[Observation.Id]).xmapL(TargetsObsPage.obsId)
        ) ~> renderP(
          targetTab
        )
        | staticRoute("/configurations", ConfigurationsPage) ~> render(UnderConstruction())
        | staticRoute("/constraints", ConstraintsPage) ~> render(UnderConstruction()))
        .notFound(redirectToPage(HomePage)(SetRouteVia.HistoryPush))
        .verify(
          HomePage,
          ProposalPage,
          ObservationsBasePage,
          ObsPage(randomId(Observation.Id.fromLong)),
          TargetsBasePage,
          TargetPage(randomId(Target.Id.fromLong)),
          TargetsObsPage(randomId(Observation.Id.fromLong)),
          ConfigurationsPage,
          ConstraintsPage
        )
        .onPostRenderP {
          case (prev, next, view)
              if prev.exists(_ =!= next) &&
                // Short circuit if we get here because of a change in the model.
                next =!= view.zoom(RootModelRouting.lens).get =>
            view.zoom(RootModelRouting.lens).set(next).runAsyncCB
          case (None, next, view) =>
            // Set the model if none was previously set
            view.zoom(RootModelRouting.lens).set(next).runAsyncCB
          case _                  => Callback.empty
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
