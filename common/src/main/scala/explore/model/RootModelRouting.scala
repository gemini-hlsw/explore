// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all._
import explore.model.FocusedObs
import explore.model.Page._
import explore.model.enum.AppTab
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.Lens

object RootModelRouting {

  protected def getPage(model: RootModel): Page =
    getPage(model.tabs.focus, model.focusedObs, model.focusedTarget)

  def getPage(tab: AppTab, focusedObs: Option[FocusedObs], focusedTarget: Option[Target.Id]): Page =
    tab match {
      case AppTab.Proposal       => ProposalPage
      case AppTab.Overview       => HomePage
      case AppTab.Observations   =>
        (focusedObs, focusedTarget) match {
          case (Some(fo), Some(targetId)) => ObsTargetPage(fo.obsId, targetId)
          case (Some(fo), _)              => ObsPage(fo.obsId)
          case _                          => ObservationsBasePage
        }
      case AppTab.Targets        =>
        (focusedObs, focusedTarget) match {
          case (Some(fo), _)       => TargetsObsPage(fo.obsId)
          case (_, Some(targetId)) => TargetPage(targetId)
          case _                   => TargetsBasePage
        }
      case AppTab.Configurations => ConfigurationsPage
      case AppTab.Constraints    =>
        focusedObs
          .map(fo => ConstraintsObsPage(fo.obsId))
          .getOrElse(ConstraintsBasePage)
    }

  protected def setTab(tab: AppTab): RootModel => RootModel =
    RootModel.tabs.modify(_.withFocus(tab))

  protected def setFocusedObs(obsId: Observation.Id): RootModel => RootModel =
    RootModel.focusedObs.replace(FocusedObs(obsId).some)

  protected val unsetFocusedObs: RootModel => RootModel =
    RootModel.focusedObs.replace(none)

  protected def setFocusedTarget(targetId: Target.Id): RootModel => RootModel =
    RootModel.focusedTarget.replace(targetId.some)

  protected val unsetFocusedTarget: RootModel => RootModel =
    RootModel.focusedTarget.replace(none)

  protected def setPage(page: Page): RootModel => RootModel =
    page match {
      case ProposalPage                   => setTab(AppTab.Proposal) >>> unsetFocusedObs
      case ObservationsBasePage           =>
        setTab(AppTab.Observations) >>> unsetFocusedObs >>> unsetFocusedTarget
      case ObsPage(obsId)                 =>
        setTab(AppTab.Observations) >>> setFocusedObs(obsId)
      case ObsTargetPage(obsId, targetId) =>
        setTab(AppTab.Observations) >>> setFocusedObs(obsId) >>> setFocusedTarget(targetId)
      case ObsAdvancedConfPage(obsId)     =>
        setTab(AppTab.Observations) >>> setFocusedObs(obsId)
      case TargetsBasePage                =>
        setTab(AppTab.Targets) >>> unsetFocusedObs >>> unsetFocusedTarget
      case TargetsObsPage(obsId)          =>
        setTab(AppTab.Targets) >>> setFocusedObs(obsId)
      case TargetPage(targetId)           =>
        setTab(AppTab.Targets) >>> unsetFocusedObs >>> setFocusedTarget(targetId)
      case ConstraintsBasePage            =>
        setTab(AppTab.Constraints)
      case ConstraintsObsPage(obsId)      =>
        setTab(AppTab.Constraints) >>> setFocusedObs(obsId)
      case ConfigurationsPage             =>
        setTab(AppTab.Configurations)
      case HomePage                       =>
        setTab(AppTab.Overview)
    }

  val lens: Lens[RootModel, Page] =
    Lens(getPage)(setPage)
}
