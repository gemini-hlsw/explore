// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.Order.*
import cats.data.NonEmptySet
import cats.syntax.all.*
import crystal.*
import crystal.react.View
import explore.model.*
import explore.model.Observation
import explore.model.Page
import explore.model.Page.*
import explore.modes.SpectroscopyModesMatrix
import explore.proposal.ProposalTabContents
import explore.tabs.*
import explore.tabs.ConstraintsTabContents
import explore.tabs.ProgramTabContents
import explore.undo.UndoContext
import japgolly.scalajs.react.React
import japgolly.scalajs.react.ReactMonocle.*
import japgolly.scalajs.react.extra.router.*
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.Gid
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given

import scala.collection.immutable.SortedSet
import scala.scalajs.LinkingInfo
import scala.util.Random

object Routing:

  private def withProgramSummaries(model: RootModelViews)(
    render: UndoContext[ProgramSummaries] => VdomNode
  ): VdomElement =
    // Not sure why the router's renderer requires VdomElement instead of VdomNode.
    // React.Fragment allows us to convert VdomNode into VdomElement.
    React.Fragment(
      model.programSummaries.throttlerView.toOptionView
        .map: (pss: View[ProgramSummaries]) =>
          render(UndoContext(model.rootModel.zoom(RootModel.undoStacks), pss))
        .toPot
        .renderPot(identity)
    )

  private def userPreferences(model: View[RootModel]): UserPreferences =
    model.zoom(RootModel.userPreferences).get.getOrElse(UserPreferences.Default)

  private def overviewTab(page: Page, model: RootModelViews): VdomElement =
    val routingInfo = RoutingInfo.from(page)
    withProgramSummaries(model)(programSummaries =>
      OverviewTabContents(
        routingInfo.programId,
        model.rootModel.zoom(RootModel.vault).get,
        programSummaries.model.zoom(ProgramSummaries.obsAttachments),
        programSummaries.model.get.obsAttachmentAssignments,
        programSummaries.get.observations,
        userPreferences(model.rootModel).overviewTabLayout,
        programSummaries.get.proposalIsSubmitted
      )
    )

  private def targetTab(page: Page, model: RootModelViews): VdomElement =
    val routingInfo = RoutingInfo.from(page)
    withProgramSummaries(model)(programSummaries =>
      model.rootModel
        .zoom(RootModel.userPreferences)
        .mapValue(userPrefs =>
          TargetTabContents(
            routingInfo.programId,
            model.rootModel.zoom(RootModel.userId).get,
            programSummaries,
            userPrefs,
            routingInfo.focused,
            model.rootModel.zoom(RootModel.searchingTarget),
            model.rootModel.zoom(RootModel.expandedIds.andThen(ExpandedIds.asterismObsIds)),
            programSummaries.get.proposalIsSubmitted
          )
        )
    )

  private def obsTab(page: Page, model: RootModelViews): VdomElement =
    val routingInfo = RoutingInfo.from(page)
    withProgramSummaries(model)(programSummaries =>
      model.rootModel
        .zoom(RootModel.userPreferences)
        .mapValue(userPrefs =>
          ObsTabContents(
            model.rootModel.zoom(RootModel.vault).get,
            routingInfo.programId,
            programSummaries,
            userPrefs,
            model.rootModel
              .zoom(RootModel.spectroscopyModes)
              .get
              .getOrElse(SpectroscopyModesMatrix.empty),
            routingInfo.focused,
            model.rootModel.zoom(RootModel.searchingTarget),
            model.rootModel.zoom(RootModel.expandedIds.andThen(ExpandedIds.obsListGroupIds))
          )
        )
    )

  private def constraintSetTab(page: Page, model: RootModelViews): VdomElement =
    val routingInfo = RoutingInfo.from(page)
    withProgramSummaries(model)(programSummaries =>
      ConstraintsTabContents(
        model.rootModel.zoom(RootModel.userId).get,
        routingInfo.programId,
        programSummaries,
        userPreferences(model.rootModel),
        routingInfo.focused.obsSet,
        model.rootModel.zoom(RootModel.expandedIds.andThen(ExpandedIds.constraintSetObsIds)),
        programSummaries.get.proposalIsSubmitted
      )
    )

  private def schedulingTab(page: Page, model: RootModelViews): VdomElement =
    withProgramSummaries(model)(programSummaries =>
      val routingInfo = RoutingInfo.from(page)
      SchedulingTabContents(
        routingInfo.programId,
        model.rootModel.zoom(RootModel.userId).get,
        programSummaries,
        userPreferences(model.rootModel),
        routingInfo.focused.obsSet,
        model.rootModel.zoom(RootModel.expandedIds.andThen(ExpandedIds.schedulingObsIds)),
        programSummaries.get.proposalIsSubmitted
      )
    )

  private def proposalTab(page: Page, model: RootModelViews): VdomElement =
    withProgramSummaries(model)(programSummaries =>
      val routingInfo = RoutingInfo.from(page)
      // if we got this far, we will have program details
      programSummaries.model
        .zoom(ProgramSummaries.optProgramDetails)
        .toOptionView
        .map(detailsView =>
          ProposalTabContents(
            routingInfo.programId,
            model.rootModel.zoom(RootModel.vault).get,
            detailsView,
            model.rootModel.zoom(RootModel.cfps).get.orEmpty,
            programSummaries.model.get.programTimesPot.map(_.timeEstimateRange),
            programSummaries.model.zoom(ProgramSummaries.proposalAttachments),
            model.rootModel.zoom(RootModel.otherUndoStacks).zoom(ModelUndoStacks.forProposal),
            userPreferences(model.rootModel).proposalTabLayout
          )
        )
    )

  private def programTab(page: Page, model: RootModelViews): VdomElement =
    withProgramSummaries(model): programSummaries =>
      val routingInfo = RoutingInfo.from(page)
      for
        programDetails <-
          programSummaries.model.zoom(ProgramSummaries.optProgramDetails).toOptionView
        proposal       <- programDetails.get.proposal
        callId         <- proposal.callId
        cfps           <- model.rootModel.get.cfps
        cfp            <- cfps.find(_.id === callId)
      yield ProgramTabContents(
        routingInfo.programId,
        programDetails,
        model.rootModel.zoom(RootModel.vault).get,
        programSummaries.get.programTimesPot,
        cfp.semester,
        userPreferences(model.rootModel)
      )

  // The programs popup will be shown
  private def noProgram: VdomElement = React.Fragment()

  def config: RouterWithPropsConfig[Page, RootModelViews] =
    RouterWithPropsConfigDsl[Page, RootModelViews].buildConfig: dsl =>
      import dsl.*

      // We can't use gid.regexPattern because capture groups don't work here.
      def gidRegEx[Id](using gid: Gid[Id]): String =
        s"${gid.tag.value.toString}-[1-9a-f][0-9a-f]*"

      def id[Id](using gid: Gid[Id]): StaticDsl.RouteB[Id] =
        string(gidRegEx).pmapL(gid.fromString)

      def idList[Id](using gid: Gid[Id]): StaticDsl.RouteB[NonEmptySet[Id]] = {
        val separator = ":"
        val regex     = s"${gidRegEx[Id]}(?:\\$separator${gidRegEx[Id]})*"
        string(regex).pmap(
          _.split(s"\\$separator").toList
            .map(s => gid.fromString.getOption(s))
            .sequence
            .map(l => NonEmptySet.fromSetUnsafe(SortedSet.from(l)))
        )(_.toList.map(_.toString).mkString(separator))
      }

      val rules =
        (emptyRule
          | staticRoute(root, NoProgramPage) ~> render(noProgram)

          | dynamicRouteCT((root / id[Program.Id]).xmapL(HomePage.iso)) ~> dynRenderP {
            case (p, m) => overviewTab(p, m)
          }

          | dynamicRouteCT(
            (root / id[Program.Id] / "program").xmapL(ProgramPage.iso)
          ) ~> dynRenderP { case (p, m) => programTab(p, m) }

          | dynamicRouteCT(
            (root / id[Program.Id] / "proposal").xmapL(ProposalPage.iso)
          ) ~> dynRenderP { case (p, m) => proposalTab(p, m) }

          | dynamicRouteCT(
            (root / id[Program.Id] / "observations").xmapL(ObservationsBasePage.iso)
          ) ~> dynRenderP { case (p, m) => obsTab(p, m) }

          | dynamicRouteCT(
            (root / id[Program.Id] / "observation" / id[Observation.Id] / "target" / id[Target.Id])
              .xmapL(ObsTargetPage.iso)
          ) ~> dynRenderP { case (p, m) => obsTab(p, m) }

          | dynamicRouteCT(
            (root / id[Program.Id] / "observation" / id[Observation.Id]).xmapL(ObsPage.iso)
          ) ~> dynRenderP { case (p, m) => obsTab(p, m) }

          | dynamicRouteCT(
            (root / id[Program.Id] / "observation" / id[Group.Id]).xmapL(ObsGroupPage.iso)
          ) ~> dynRenderP { case (p, m) => obsTab(p, m) }

          | dynamicRouteCT((root / id[Program.Id] / "targets").xmapL(TargetsBasePage.iso)) ~>
          dynRenderP { case (p, m) => targetTab(p, m) }

          | dynamicRouteCT((root / id[Program.Id] / "target" / id[Target.Id]).xmapL(TargetPage.iso))
          ~> dynRenderP { case (p, m) => targetTab(p, m) }

          | dynamicRouteCT(
            (root / id[Program.Id] / "targets/obs" / idList[Observation.Id] / "target" / id[
              Target.Id
            ])
              .xmapL(TargetWithObsPage.iso)
          ) ~> dynRenderP { case (p, m) => targetTab(p, m) }

          | dynamicRouteCT(
            (root / id[Program.Id] / "targets/obs" / idList[Observation.Id])
              .xmapL(TargetsObsPage.iso)
          ) ~> dynRenderP { case (p, m) => targetTab(p, m) }

          | dynamicRouteCT(
            (root / id[Program.Id] / "constraints").xmapL(ConstraintsBasePage.iso)
          ) ~> dynRenderP { case (p, m) => constraintSetTab(p, m) }

          | dynamicRouteCT(
            (root / id[Program.Id] / "constraints/obs" / idList[Observation.Id])
              .xmapL(ConstraintsObsPage.iso)
          ) ~> dynRenderP { case (p, m) => constraintSetTab(p, m) }

          | dynamicRouteCT(
            (root / id[Program.Id] / "scheduling").xmapL(SchedulingBasePage.iso)
          ) ~> dynRenderP { case (p, m) => schedulingTab(p, m) }

          | dynamicRouteCT(
            (root / id[Program.Id] / "scheduling/obs" / idList[Observation.Id])
              .xmapL(SchedulingObsPage.iso)
          ) ~> dynRenderP { case (p, m) => schedulingTab(p, m) })

      val configuration =
        rules
          .notFound(redirectToPage(NoProgramPage)(SetRouteVia.HistoryPush))
          .renderWithP((_, resolution) => ExploreLayout(resolution))
      // .logToConsole

      // Only link and run this in dev mode. Works since calling `verify` trigger verification immediately.
      if (LinkingInfo.developmentMode) {
        def randomId[Id](fromLong: Long => Option[Id]): Id =
          fromLong(Random.nextLong().abs).get

        def pid      = randomId(Program.Id.fromLong)
        def oid      = randomId(Observation.Id.fromLong)
        def tid      = randomId(Target.Id.fromLong)
        def oneObs   = ObsIdSet.one(oid)
        def twoObs   = oneObs.add(oid)
        def threeObs = oneObs.add(oid)

        configuration
          .verify(
            NoProgramPage,
            HomePage(pid),
            ProgramPage(pid),
            ProposalPage(pid),
            ObservationsBasePage(pid),
            ObsPage(pid, oid),
            ObsTargetPage(pid, oid, tid),
            TargetsBasePage(pid),
            TargetWithObsPage(pid, oneObs, tid),
            TargetWithObsPage(pid, twoObs, tid),
            TargetWithObsPage(pid, threeObs, tid),
            TargetsObsPage(pid, oneObs),
            TargetsObsPage(pid, twoObs),
            TargetsObsPage(pid, threeObs),
            TargetPage(pid, tid),
            // ConfigurationsPage(pid),
            ConstraintsBasePage(pid),
            ConstraintsObsPage(pid, oneObs),
            ConstraintsObsPage(pid, twoObs),
            ConstraintsObsPage(pid, threeObs),
            SchedulingBasePage(pid),
            SchedulingObsPage(pid, oneObs),
            SchedulingObsPage(pid, twoObs),
            SchedulingObsPage(pid, threeObs)
          )
        ()
      }

      configuration
