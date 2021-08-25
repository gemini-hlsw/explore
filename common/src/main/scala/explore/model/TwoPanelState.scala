// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import japgolly.scalajs.react.ReactCats._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.facade.JsNumber
import lucuma.ui.reusability._
import monocle.Focus
import org.scalajs.dom.window

sealed abstract class SelectedPanel[+A] extends Product with Serializable {
  import SelectedPanel._
  def rightPanelVisible: Boolean = this match {
    case Uninitialized => false
    case Tree          => false
    case Summary       => true
    case Editor(_)     => true
  }

  def leftPanelVisible: Boolean = !rightPanelVisible

  def optValue: Option[A] = this match {
    case Editor(value) => value.some
    case _             => none
  }
}

object SelectedPanel {
  case object Uninitialized extends SelectedPanel[Nothing]
  case object Tree          extends SelectedPanel[Nothing]
  case object Summary       extends SelectedPanel[Nothing]
  case class Editor[A](value: A) extends SelectedPanel[A]

  def unitialized[A]: SelectedPanel[A] = Uninitialized
  def tree[A]: SelectedPanel[A]    = Tree
  def summary[A]: SelectedPanel[A] = Summary
  def editor[A](value: A): SelectedPanel[A] = Editor(value)

  implicit def eqSelectedPanel[A: Eq]: Eq[SelectedPanel[A]] = Eq.instance {
    case (Uninitialized, Uninitialized) => true
    case (Tree, Tree)                   => true
    case (Summary, Summary)             => true
    case (Editor(a), Editor(b))         => a === b
    case _                              => false
  }

  implicit def reuseSelectedPanel[A: Eq]: Reusability[SelectedPanel[A]] = Reusability.byEq
}

final case class TwoPanelState[A](treeWidth: JsNumber, selected: SelectedPanel[A])

object TwoPanelState {
  def selected[A]  = Focus[TwoPanelState[A]](_.selected)
  def treeWidth[A] = Focus[TwoPanelState[A]](_.treeWidth)

  // Keep them as def to take the value window.innerWidth at the current time
  def isTwoPanel: Boolean = window.innerWidth > Constants.TwoPanelCutoff

  def initialPanelWidth[A](sp: SelectedPanel[A]): Double =
    if (isTwoPanel) Constants.InitialTreeWidth
    else if (sp.rightPanelVisible) 0
    else window.innerWidth

  def initial[A](sp: SelectedPanel[A]): TwoPanelState[A] =
    TwoPanelState(initialPanelWidth(sp), sp)

  implicit def stateReuse[A](implicit ev: Eq[A]): Reusability[TwoPanelState[A]] =
    Reusability.by(tps => (tps.treeWidth, tps.selected))
}
