// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.implicits._
import cats.kernel.Eq
import explore.model.Page._
import explore.model.enum.AppTab
import gpp.util.Zipper
import monocle.Lens

object RootModelRouting {

  protected def getPage(model: RootModel): Page =
    model.tabs.focus match {
      case AppTab.Overview       => HomePage
      case AppTab.Observations   => model.obsId.map(ObsPage.apply).getOrElse(HomePage)
      case AppTab.Targets        => model.obsId.map(TargetPage.apply).getOrElse(HomePage)
      case AppTab.Configurations => ConfigurationsPage
      case AppTab.Constraints    => ConstraintsPage
    }

  protected def setTab(tab: AppTab): RootModel => RootModel =
    RootModel.tabs.modify(_.withFocus(tab))

  protected def setPage(page: Page): RootModel => RootModel =
    page match {
      case ObsPage(obsId)     =>
        setTab(AppTab.Observations) >>> (RootModel.obsId.set(obsId.some))
      case TargetPage(obsId)  =>
        setTab(AppTab.Targets) >>> (RootModel.obsId.set(obsId.some))
      case ConstraintsPage    =>
        setTab(AppTab.Constraints)
      case ConfigurationsPage =>
        setTab(AppTab.Configurations)
      case HomePage           =>
        setTab(AppTab.Overview)
    }

  val lens: Lens[RootModel, Page] =
    Lens(getPage)(setPage)
}
