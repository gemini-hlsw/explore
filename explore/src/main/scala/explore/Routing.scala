// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.SyncIO
import cats.syntax.all._
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
import scala.annotation.unused

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

  private def targetTab(@unused model: View[RootModel]): VdomElement =
    <.div("Target Tab")
  // withSize { size =>
  //   AppCtx.using(implicit ctx =>
  //     TargetTabContents(
  //       model.zoom(RootModel.userId).get,
  //       model.zoom(RootModel.focused),
  //       model.zoom(RootModel.undoStacks).zoom(ModelUndoStacks.forTargetList),
  //       model.zoom(RootModel.undoStacks).zoom(ModelUndoStacks.forTarget),
  //       model.zoom(RootModel.searchingTarget),
  //       model.zoom(RootModel.expandedIds),
  //       model.zoom(RootModel.targetSummaryHiddenColumns),
  //       size
  //     )
  //   )
  // }

  private def obsTab(model: View[RootModel]): VdomElement =
    withSize(size =>
      AppCtx.using(implicit ctx =>
        ObsTabContents(model.zoom(RootModel.userId),
                       model.zoom(RootModel.focused),
                       model.zoom(RootModel.undoStacks),
                       model.zoom(RootModel.searchingTarget),
                       size
        )
      )
    )

  private def constraintSetTab(model: View[RootModel]): VdomElement =
    withSize(size =>
      AppCtx.using(implicit ctx =>
        ConstraintSetTabContents(
          model.zoom(RootModel.userId).get,
          model.zoom(RootModel.focused),
          model.zoom(
            RootModel.expandedIds.andThen(ExpandedIds.constraintSetObsIds)
          ),
          model.zoom(RootModel.undoStacks).zoom(ModelUndoStacks.forConstraintList),
          model.zoom(RootModel.undoStacks).zoom(ModelUndoStacks.forBulkConstraintSet),
          model.zoom(RootModel.constraintSummaryHiddenColumns),
          model.zoom(RootModel.constraintSummarySorting),
          size
        )
      )
    )

  def config: RouterWithPropsConfig[Page, View[RootModel]] =
    RouterWithPropsConfigDsl[Page, View[RootModel]].buildConfig { dsl =>
      import dsl._

      def id[Id](implicit gid: Gid[Id]): StaticDsl.RouteB[Id] =
        string(gid.regexPattern).pmapL(gid.fromString)

      val rules =
        (emptyRule
          | staticRoute(root, HomePage) ~> render(UnderConstruction())
          | staticRoute("/proposal", ProposalPage) ~> render(
            AppCtx.using(implicit ctx => ProposalTabContents())
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
            ("/target/obs" / id[Observation.Id]).xmapL(TargetsObsPage.obsId)
          ) ~> renderP(
            targetTab
          )
          | staticRoute("/configurations", ConfigurationsPage) ~> render(SequenceEditor())
          | staticRoute("/constraints", ConstraintsBasePage) ~> renderP(constraintSetTab))

      val configuration =
        rules
          .notFound(redirectToPage(HomePage)(SetRouteVia.HistoryPush))
          .onPostRenderP {
            case (prev, next, view)
                if prev.exists(_ =!= next) &&
                  // Short circuit if we get here because of a change in the model.
                  next =!= view.zoom(RootModelRouting.lens).get =>
              view.zoom(RootModelRouting.lens).set(next)
            case (None, next, view) =>
              // Set the model if none was previously set
              view.zoom(RootModelRouting.lens).set(next)
            case _                  => SyncIO.unit
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
