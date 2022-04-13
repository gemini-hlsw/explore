// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import japgolly.scalajs.react.ReactCats._
import japgolly.scalajs.react.Reusability
import lucuma.ui.reusability._
import monocle.Focus
import org.scalajs.dom.window

sealed abstract class SelectedPanel extends Product with Serializable {
  import SelectedPanel._
  def rightPanelVisible: Boolean = this match {
    case Uninitialized => false
    case Tree          => false
    case Summary       => true
    case Editor        => true
  }

  def leftPanelVisible: Boolean = !rightPanelVisible
}

object SelectedPanel {
  case object Uninitialized extends SelectedPanel
  case object Tree          extends SelectedPanel
  case object Summary       extends SelectedPanel
  case object Editor        extends SelectedPanel

  def unitialized: SelectedPanel = Uninitialized
  def tree: SelectedPanel        = Tree
  def summary: SelectedPanel     = Summary
  def editor: SelectedPanel      = Editor

  implicit def eqSelectedPanel: Eq[SelectedPanel] = Eq.instance {
    case (Uninitialized, Uninitialized) => true
    case (Tree, Tree)                   => true
    case (Summary, Summary)             => true
    case (Editor, Editor)               => true
    case _                              => false
  }

  implicit val reuseSelectedPanel: Reusability[SelectedPanel] = Reusability.byEq
}

final case class TwoPanelState(treeWidth: Double, selected: SelectedPanel)

object TwoPanelState {
  val selected  = Focus[TwoPanelState](_.selected)
  val treeWidth = Focus[TwoPanelState](_.treeWidth)

  // Keep them as def to take the value window.innerWidth at the current time
  def isTwoPanel: Boolean = window.innerWidth > Constants.TwoPanelCutoff

  def initialPanelWidth(sp: SelectedPanel): Double =
    if (isTwoPanel) Constants.InitialTreeWidth
    else if (sp.rightPanelVisible) 0
    else window.innerWidth

  def initial(sp: SelectedPanel): TwoPanelState =
    TwoPanelState(initialPanelWidth(sp), sp)

  private implicit def doubleReuse = Reusability.double(1.0)

  implicit val stateReuse: Reusability[TwoPanelState] =
    Reusability.by(tps => (tps.treeWidth, tps.selected))
}
