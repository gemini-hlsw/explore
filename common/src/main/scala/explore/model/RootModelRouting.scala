package explore.model

import cats.implicits._
import monocle.Lens
import explore.model.enum.AppTab
import explore.model.Page._
import cats.kernel.Eq
import gpp.util.Zipper

object RootModelRouting {

  protected def getPage(model: RootModel): Page = {
    val pageOpt =
      model.tabs.focus match {
        case AppTab.Observations => model.obsId.map(ObsPage.apply)
        case _                   => none
      }
    pageOpt.getOrElse(HomePage)
  }

  protected def findEqFocus[A: Eq](zipper: Zipper[A], a: A): Option[Zipper[A]] =
    zipper.findFocus(_ === a)

  protected def setPage(page: Page): RootModel => RootModel =
    model => {
      val modOpt =
        page match {
          case ObsPage(obsId) =>
            findEqFocus(RootModel.tabs.get(model), AppTab.Observations).map(z =>
              RootModel.tabs.set(z) >>> RootModel.obsId.set(obsId.some)
            )
          case HomePage       =>
            findEqFocus(RootModel.tabs.get(model), AppTab.Overview).map(z => RootModel.tabs.set(z))
          case _              => none
        }
      modOpt.getOrElse(identity[RootModel] _)(model)
    }

  val lens: Lens[RootModel, Page] =
    Lens(getPage)(setPage)
}
