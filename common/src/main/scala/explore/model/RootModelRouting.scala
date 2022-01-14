// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all._
import explore.model.FocusedObs
import explore.model.Page._
import explore.model.enum.AppTab
import monocle.Lens

object RootModelRouting {

  protected def getPage(model: RootModel): Page =
    getPage(model.tabs.focus, model.focusedObs)

  def getPage(tab: AppTab, focusedObs: Option[FocusedObs]): Page =
    tab match {
      case AppTab.Proposal       => ProposalPage
      case AppTab.Overview       => HomePage
      case AppTab.Observations   =>
        focusedObs
          .map(fo => ObsPage(fo.obsId))
          .getOrElse(ObservationsBasePage)
      case AppTab.Targets        =>
        focusedObs
          .map(fo => TargetsObsPage(fo.obsId))
          .getOrElse(TargetsBasePage)
      case AppTab.Configurations => ConfigurationsPage
      case AppTab.Constraints    =>
        focusedObs
          .map(fo => ConstraintsObsPage(fo.obsId))
          .getOrElse(ConstraintsBasePage)
    }

  protected def setTab(tab: AppTab): RootModel => RootModel =
    RootModel.tabs.modify(_.withFocus(tab))

  protected def setPage(page: Page): RootModel => RootModel =
    page match {
      case ProposalPage               => setTab(AppTab.Proposal) >>> RootModel.focusedObs.replace(none)
      case ObservationsBasePage       =>
        setTab(AppTab.Observations) >>> RootModel.focusedObs.replace(none)
      case ObsPage(obsId)             =>
        setTab(AppTab.Observations) >>> RootModel.focusedObs.replace(FocusedObs(obsId).some)
      case ObsAdvancedConfPage(obsId) =>
        setTab(AppTab.Observations) >>> RootModel.focusedObs.replace(FocusedObs(obsId).some)
      case TargetsBasePage            =>
        setTab(AppTab.Targets) >>> RootModel.focusedObs.replace(none)
      case TargetsObsPage(obsId)      =>
        setTab(AppTab.Targets) >>> RootModel.focusedObs.replace(FocusedObs(obsId).some)
      case ConstraintsBasePage        =>
        setTab(AppTab.Constraints)
      case ConstraintsObsPage(obsId)  =>
        setTab(AppTab.Constraints) >>> RootModel.focusedObs.replace(FocusedObs(obsId).some)
      case ConfigurationsPage         =>
        setTab(AppTab.Configurations)
      case HomePage                   =>
        setTab(AppTab.Overview)
    }

  val lens: Lens[RootModel, Page] =
    Lens(getPage)(setPage)
}
