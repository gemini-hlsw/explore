// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import cats.data.NonEmptyList
import lucuma.core.util.Enumerated
import monocle.Iso

/**
 * Enum to indicate visibility of an item, roughly equivalent to css display
 */
sealed trait Visible extends Product with Serializable {
  def fold[A](hidden: => A, inline: => A): A =
    this match {
      case Visible.Hidden => hidden
      case Visible.Inline => inline
    }

  def visible: Boolean = fold(false, true)

  def flip: Visible = fold(Visible.Inline, Visible.Hidden)
}

object Visible {
  case object Hidden extends Visible
  case object Inline extends Visible

  val boolIso: Iso[Boolean, Visible] = Iso[Boolean, Visible] { b =>
    if (b) Inline else Hidden
  } {
    case Hidden => false
    case Inline => true
  }

  val boolReverseIso: Iso[Visible, Boolean] = boolIso.reverse

  val all = NonEmptyList.of(Hidden, Inline)

  /** @group Typeclass Instances */
  implicit val DisplayEnumerated: Enumerated[Visible] =
    Enumerated.of(all.head, all.tail: _*)
}
