package explore.syntax

import org.scalajs.dom.Window
import explore.model.Constants

package object ui {
  implicit class WindowOps(val self: Window) extends AnyVal {
    def canFitTwoPanels: Boolean =
      self.innerWidth <= Constants.TwoPanelCutoff
  }
}
