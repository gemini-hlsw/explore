// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.implicits._
import cats.kernel.Eq
import explore.model.Focused.FocusedObs
import explore.model.Page._
import explore.model.enum.AppTab
import gem.data.Zipper
import monocle.Lens

object RootModelRouting {

  protected def getPage(model: RootModel): Page =
    model.tabs.focus match {
      case AppTab.Overview       => HomePage
      case AppTab.Observations   =>
        RootModel.focusedObsId
          .getOption(model)
          .map(ObsPage.apply)
          .getOrElse(HomePage)
      case AppTab.Targets        =>
        RootModel.focusedTargetOrObsId
          .getOption(model)
          .map(_.fold(TargetPage.apply, TargetsObsPage.apply))
          .getOrElse(HomePage)
      case AppTab.Configurations => ConfigurationsPage
      case AppTab.Constraints    => ConstraintsPage
    }

  protected def setTab(tab: AppTab): RootModel => RootModel =
    RootModel.tabs.modify(_.withFocus(tab))

  protected def setPage(page: Page): RootModel => RootModel =
    page match {
      case ObsPage(obsId)        =>
        setTab(AppTab.Observations) >>> (RootModel.focusedObsId.set(obsId))
      case TargetPage(targetId)  =>
        setTab(AppTab.Targets) >>> (RootModel.focusedTargetId.set(targetId))
      case TargetsObsPage(obsId) =>
        setTab(AppTab.Targets) >>> (RootModel.focusedObsId.set(obsId))
      case ConstraintsPage       =>
        setTab(AppTab.Constraints)
      case ConfigurationsPage    =>
        setTab(AppTab.Configurations)
      case HomePage              =>
        setTab(AppTab.Overview)
    }

  val lens: Lens[RootModel, Page] =
    Lens(getPage)(setPage)
}
