// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all._
import crystal.react.ReuseView
import explore.components.ui.ExploreStyles
import explore.config.SequenceEditor
import explore.model.Page
import explore.model.Page._
import explore.model._
import explore.proposal._
import explore.tabs._
import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.util.Gid
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

  private def targetTab(page: Page, model: ReuseView[RootModel]): VdomElement =
    AppCtx.using { implicit ctx =>
      val routingInfo = RoutingInfo.from(page)
      TargetTabContents(
        model.zoom(RootModel.userId).get,
        routingInfo.focusedObs,
        routingInfo.focusedTarget,
        model.zoom(RootModel.undoStacks).zoom(ModelUndoStacks.forAsterismGroupList),
        model.zoom(RootModel.undoStacks).zoom(ModelUndoStacks.forSiderealTarget),
        model.zoom(RootModel.searchingTarget),
        model.zoom(RootModel.expandedIds.andThen(ExpandedIds.asterismObsIds)),
        model.zoom(RootModel.targetSummaryHiddenColumns)
      )
    }

  private def obsTab(page: Page, model: ReuseView[RootModel]): VdomElement =
    AppCtx.using { implicit ctx =>
      val routingInfo = RoutingInfo.from(page)
      ObsTabContents(
        model.zoom(RootModel.userId),
        routingInfo.focusedObs,
        routingInfo.focusedTarget,
        model.zoom(RootModel.undoStacks),
        model.zoom(RootModel.searchingTarget),
        model.zoom(RootModel.targetSummaryHiddenColumns)
      )
    }

  private def constraintSetTab(page: Page, model: ReuseView[RootModel]): VdomElement =
    withSize(size =>
      AppCtx.using(implicit ctx =>
        ConstraintSetTabContents(
          model.zoom(RootModel.userId).get,
          RoutingInfo.from(page).focusedObs,
          model.zoom(
            RootModel.expandedIds.andThen(ExpandedIds.constraintSetObsIds)
          ),
          model.zoom(RootModel.undoStacks).zoom(ModelUndoStacks.forConstraintList),
          model.zoom(RootModel.undoStacks).zoom(ModelUndoStacks.forConstraintGroup),
          model.zoom(RootModel.constraintSummaryHiddenColumns),
          model.zoom(RootModel.constraintSummarySorting),
          size
        )
      )
    )

  def config: RouterWithPropsConfig[Page, ReuseView[RootModel]] =
    RouterWithPropsConfigDsl[Page, ReuseView[RootModel]].buildConfig { dsl =>
      import dsl._

      def id[Id](implicit gid: Gid[Id]): StaticDsl.RouteB[Id] =
        // We can't use gid.regexPattern because capture groups don't work here.
        string(s"${gid.tag.value.toString}-[1-9a-f][0-9a-f]*").pmapL(gid.fromString)

      val rules =
        (emptyRule
          | staticRoute(root, HomePage) ~> render(UnderConstruction())
          | staticRoute("/proposal", ProposalPage) ~> render(
            AppCtx.using(implicit ctx => ProposalTabContents())
          )
          | staticRoute("/observations", ObservationsBasePage) ~> renderP(
            obsTab(ObservationsBasePage, _)
          )
          | dynamicRouteCT(
            ("/observation" / id[Observation.Id] / "target" / id[Target.Id])
              .xmapL(ObsTargetPage.iso)
          ) ~> dynRenderP { case (p, m) => obsTab(p, m) }
          | dynamicRouteCT(
            ("/observation" / id[Observation.Id]).xmapL(ObsPage.obsId)
          ) ~> dynRenderP { case (p, m) => obsTab(p, m) }
          | staticRoute("/targets", TargetsBasePage) ~> renderP(targetTab(TargetsBasePage, _))
          | dynamicRouteCT(
            ("/target" / id[Target.Id]).xmapL(TargetPage.targetId)
          ) ~> dynRenderP { case (p, m) => targetTab(p, m) }
          | dynamicRouteCT(
            ("/targets/obs" / id[Observation.Id] / "target" / id[Target.Id])
              .xmapL(TargetWithObsPage.iso)
          ) ~> dynRenderP { case (p, m) => targetTab(p, m) }
          | dynamicRouteCT(
            ("/targets/obs" / id[Observation.Id]).xmapL(TargetsObsPage.obsId)
          ) ~> dynRenderP { case (p, m) => targetTab(p, m) }
          | staticRoute("/configurations", ConfigurationsPage) ~> render(SequenceEditor())
          | staticRoute("/constraints", ConstraintsBasePage) ~> renderP {
            constraintSetTab(ConstraintsBasePage, _)
          }
          | dynamicRouteCT(
            ("/constraints/obs" / id[Observation.Id]).xmapL(ConstraintsObsPage.obsId)
          ) ~> dynRenderP { case (p, m) => constraintSetTab(p, m) })

      val configuration =
        rules
          .notFound(redirectToPage(HomePage)(SetRouteVia.HistoryPush))
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
            ObsTargetPage(randomId(Observation.Id.fromLong), randomId(Target.Id.fromLong)),
            TargetsBasePage,
            TargetWithObsPage(randomId(Observation.Id.fromLong), randomId(Target.Id.fromLong)),
            TargetsObsPage(randomId(Observation.Id.fromLong)),
            TargetPage(randomId(Target.Id.fromLong)),
            ConfigurationsPage,
            ConstraintsBasePage
          )
      }

      configuration
    }

  private def layout(
    c: RouterCtl[Page],
    r: ResolutionWithProps[Page, ReuseView[RootModel]]
  ): ReuseView[RootModel] => VdomElement =
    ExploreLayout(c, r)
}
