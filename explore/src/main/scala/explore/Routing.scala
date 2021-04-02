// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import explore.components.ui.ExploreStyles
import explore.model.Page
import explore.model.Page._
import explore.model._
import explore.proposal._
import explore.tabs._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Asterism
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.util.Gid
import org.typelevel.log4cats.Logger
import react.resizeDetector.ResizeDetector

import scala.scalajs.LinkingInfo
import scala.util.Random

sealed trait ElementItem  extends Product with Serializable
case object IconsElement  extends ElementItem
case object LabelsElement extends ElementItem

object Routing {

  private def withSize(f: ResizeDetector.Dimensions => VdomElement): VdomElement =
    ResizeDetector() { s =>
      <.div(ExploreStyles.SizeDetector, s.targetRef)(
        f(s)
      )
    }

  private def targetTab(model: View[RootModel]): VdomElement =
    withSize { size =>
      AppCtx.using(implicit ctx =>
        TargetTabContents(
          model.zoom(RootModel.userId),
          model.zoom(RootModel.focused),
          model.zoom(RootModel.searchingTarget),
          model.zoom(RootModel.expandedIds),
          size
        )
      )
    }

  private def obsTab(model: View[RootModel]): VdomElement =
    withSize(size =>
      AppCtx.using(implicit ctx =>
        ObsTabContents(model.zoom(RootModel.userId),
                       model.zoom(RootModel.focused),
                       model.zoom(RootModel.searchingTarget),
                       size
        )
      )
    )

  private def constraintSetTab(model: View[RootModel]): VdomElement =
    withSize(size =>
      AppCtx.using(implicit ctx =>
        ConstraintSetTabContents(
          model.zoom(RootModel.userId),
          model.zoom(RootModel.focused),
          model.zoom(
            RootModel.expandedIds.composeLens(ExpandedIds.constraintSetIds)
          ),
          size
        )
      )
    )

  def config(implicit logger: Logger[IO]): RouterWithPropsConfig[Page, View[RootModel]] =
    RouterWithPropsConfigDsl[Page, View[RootModel]].buildConfig { dsl =>
      import dsl._

      def id[Id](implicit gid: Gid[Id]): StaticDsl.RouteB[Id] =
        string(gid.regexPattern).pmapL(gid.fromString)

      val rules =
        (emptyRule
          | staticRoute(root, HomePage) ~> render(UnderConstruction())
          | staticRoute("/proposal", ProposalPage) ~> renderP(view =>
            ProposalTabContents(view.zoom(RootModel.focused))
          )
          | staticRoute("/observations", ObservationsBasePage) ~> renderP(obsTab)
          | dynamicRouteCT(("/observation" / id[Observation.Id]).xmapL(ObsPage.obsId)) ~> renderP(
            obsTab
          )
          | staticRoute("/targets", TargetsBasePage) ~> renderP(targetTab)
          | dynamicRouteCT(("/target" / id[Target.Id]).xmapL(TargetPage.targetId)) ~> renderP(
            targetTab
          )
          | dynamicRouteCT(
            ("/asterism" / id[Asterism.Id]).xmapL(TargetsAsterismPage.asterismId)
          ) ~> renderP(
            targetTab
          )
          | dynamicRouteCT(
            ("/target/obs" / id[Observation.Id]).xmapL(TargetsObsPage.obsId)
          ) ~> renderP(
            targetTab
          )
          | staticRoute("/configurations", ConfigurationsPage) ~> render(UnderConstruction())
          | staticRoute("/constraints", ConstraintsBasePage) ~> renderP(constraintSetTab)
          | dynamicRouteCT(
            ("/constraint" / id[ConstraintSet.Id]).xmapL(ConstraintsPage.csId)
          ) ~> renderP(constraintSetTab)
          | dynamicRouteCT(
            ("/constraint/obs" / id[Observation.Id]).xmapL(ConstraintsObsPage.obsId)
          ) ~> renderP(constraintSetTab))

      val configuration =
        rules
          .notFound(redirectToPage(HomePage)(SetRouteVia.HistoryPush))
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
      // .logToConsole

      // Only link and run this in dev mode. Works since calling `verify` trigger verification immediately.
      if (LinkingInfo.developmentMode) {
        def randomId[Id](fromLong: Long => Option[Id]): Id =
          fromLong(Random.nextLong().abs).get

        configuration
          .verify(
            HomePage,
            ProposalPage,
            ObservationsBasePage,
            ObsPage(randomId(Observation.Id.fromLong)),
            TargetsBasePage,
            TargetPage(randomId(Target.Id.fromLong)),
            TargetsObsPage(randomId(Observation.Id.fromLong)),
            ConfigurationsPage,
            ConstraintsBasePage
          )
      }

      configuration
    }

  private def layout(
    c: RouterCtl[Page],
    r: ResolutionWithProps[Page, View[RootModel]]
  ): View[RootModel] => VdomElement =
    ExploreLayout(c, r)
}
