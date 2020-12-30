// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.raw.JsNumber
import lucuma.ui.reusability._
import monocle.macros.Lenses
import org.scalajs.dom.window

@Lenses
final case class TwoPanelState(treeWidth: JsNumber, elementSelected: Boolean) {
  val leftPanelVisible: Boolean  = !elementSelected
  val rightPanelVisible: Boolean = elementSelected
}

object TwoPanelState {
  // Keep them as def to take the value window.innerWidth at the current time
  def isTwoPanel: Boolean =
    window.innerWidth > Constants.TwoPanelCutoff

  def initialPanelWidth(v: Boolean): Double =
    if (isTwoPanel) Constants.InitialTreeWidth
    else if (v) 0
    else window.innerWidth

  def initial(v: Boolean): TwoPanelState =
    TwoPanelState(initialPanelWidth(v), v)

  implicit val stateReuse: Reusability[TwoPanelState] = Reusability.derive
}
