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
import lucuma.core.model.WithId
import monocle.Prism

sealed trait ElementItem  extends Product with Serializable
case object IconsElement  extends ElementItem
case object LabelsElement extends ElementItem

object Routing {

  private def idPrism[Id <: WithId#Id, P <: Page](
    idFromLong: Long => Either[String, Id],
    pageFromId: Id => P,
    idFromPage: P => Id
  ): Prism[Long, P] =
    Prism[Long, P](l => idFromLong(l).toOption.map(pageFromId))(idFromPage(_).value.value)

  private val obsPage: Prism[Long, ObsPage] =
    idPrism(Observation.Id.fromLong, ObsPage.apply, _.obsId)

  private val targetPage: Prism[Long, TargetPage] =
    idPrism(Target.Id.fromLong, TargetPage.apply, _.targetId)

  private val targetObsPage: Prism[Long, TargetsObsPage] =
    idPrism(Observation.Id.fromLong, TargetsObsPage.apply, _.obsId)

  private def randomId[Id](fromLong: Long => Either[String, Id]): Id =
    fromLong(Random.nextLong().abs.toLong).toOption.get

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
        | dynamicRouteCT(("/obs" / long).pmapL(obsPage)) ~> renderP(view =>
          ObsTabContents(view.zoom(RootModel.focused))
        )
        | staticRoute("/targets", TargetsBasePage) ~> renderP(view =>
          TargetTabContents(view.zoom(RootModel.focused))
        )
        | dynamicRouteCT(("/target" / long).pmapL(targetPage)) ~> renderP(view =>
          TargetTabContents(view.zoom(RootModel.focused))
        )
        | dynamicRouteCT(("/target/obs" / long).pmapL(targetObsPage)) ~> renderP(view =>
          TargetTabContents(view.zoom(RootModel.focused))
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
