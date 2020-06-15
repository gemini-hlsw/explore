// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.implicits._
import monocle.Lens
import explore.model.enum.AppTab
import explore.model.Page._
import cats.kernel.Eq
import gpp.util.Zipper

object RootModelRouting {

  protected def getPage(model: RootModel): Page =
    model.tabs.focus match {
      case AppTab.Overview       => HomePage
      case AppTab.Observations   => model.obsId.map(ObsPage.apply).getOrElse(HomePage)
      case AppTab.Targets        => HomePage
      case AppTab.Configurations => HomePage
      case AppTab.Constraints    => ConstraintsPage
    }

  protected def findEqFocus[A: Eq](zipper: Zipper[A], a: A): Option[Zipper[A]] =
    zipper.findFocus(_ === a)

  protected def setTab(tab: AppTab)(model: RootModel): Option[RootModel] =
    findEqFocus(RootModel.tabs.get(model), tab).map(z => RootModel.tabs.set(z)(model))

  protected def setPage(page: Page): RootModel => RootModel =
    model => {
      val modOpt =
        page match {
          case ObsPage(obsId)  =>
            setTab(AppTab.Observations)(model).map(RootModel.obsId.set(obsId.some))
          case ConstraintsPage =>
            setTab(AppTab.Constraints)(model)
          case HomePage        =>
            setTab(AppTab.Overview)(model)
        }
      modOpt.getOrElse(model)
    }

  val lens: Lens[RootModel, Page] =
    Lens(getPage)(setPage)
}
