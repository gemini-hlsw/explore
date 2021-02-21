// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all._
import explore.model.Focused.FocusedAsterism
import explore.model.Focused.FocusedObs
import explore.model.Focused.FocusedTarget
import explore.model.Page._
import explore.model.enum.AppTab
import monocle.Lens

object RootModelRouting {

  protected def getPage(model: RootModel): Page =
    getPage(model.tabs.focus, model.focused)

  def getPage(tab: AppTab, focused: Option[Focused]): Page =
    tab match {
      case AppTab.Proposal       => ProposalPage
      case AppTab.Overview       => HomePage
      case AppTab.Observations   =>
        focused
          .collect { case FocusedObs(obsId) => ObsPage(obsId) }
          .getOrElse(ObservationsBasePage)
      case AppTab.Targets        =>
        focused
          .collect {
            case FocusedObs(obsId)           => TargetsObsPage(obsId)
            case FocusedAsterism(asterismId) => TargetsAsterismPage(asterismId)
            case FocusedTarget(targetId)     => TargetPage(targetId)
          }
          .getOrElse(TargetsBasePage)
      case AppTab.Configurations => ConfigurationsPage
      case AppTab.Constraints    => ConstraintsPage
    }

  protected def setTab(tab: AppTab): RootModel => RootModel =
    RootModel.tabs.modify(_.withFocus(tab))

  protected def setPage(page: Page): RootModel => RootModel =
    page match {
      case ProposalPage                    => setTab(AppTab.Proposal) >>> RootModel.focused.set(none)
      case ObservationsBasePage            =>
        setTab(AppTab.Observations) >>> RootModel.focused.set(none)
      case ObsPage(obsId)                  =>
        setTab(AppTab.Observations) >>> RootModel.focused.set(FocusedObs(obsId).some)
      case TargetsBasePage                 =>
        setTab(AppTab.Targets) >>> RootModel.focused.set(none)
      case TargetPage(targetId)            =>
        setTab(AppTab.Targets) >>> RootModel.focused.set(FocusedTarget(targetId).some)
      case TargetsAsterismPage(asterismId) =>
        setTab(AppTab.Targets) >>> RootModel.focused.set(FocusedAsterism(asterismId).some)
      case TargetsObsPage(obsId)           =>
        setTab(AppTab.Targets) >>> RootModel.focused.set(FocusedObs(obsId).some)
      case ConstraintsPage                 =>
        setTab(AppTab.Constraints)
      case ConfigurationsPage              =>
        setTab(AppTab.Configurations)
      case HomePage                        =>
        setTab(AppTab.Overview)
    }

  val lens: Lens[RootModel, Page] =
    Lens(getPage)(setPage)
}
