// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import cats.data.NonEmptyList
import gem.util.Enumerated
import monocle.Iso

/**
  * Enum to indicate visibility of an item, roughly equivalent to css display
  */
sealed trait Display extends Product with Serializable {
  def fold[A](hidden: => A, inline: => A): A =
    this match {
      case Display.Hidden => hidden
      case Display.Inline => inline
    }

  def visible: Boolean = fold(false, true)
}

object Display {
  case object Hidden extends Display
  case object Inline extends Display

  val boolIso: Iso[Boolean, Display] = Iso[Boolean, Display] { b =>
    if (b) Inline else Hidden
  } {
    case Hidden => false
    case Inline => true
  }

  val boolReverseIso: Iso[Display, Boolean] = boolIso.reverse

  val all = NonEmptyList.of(Hidden, Inline)

  /** @group Typeclass Instances */
  implicit val DisplayEnumerated: Enumerated[Display] =
    Enumerated.of(all.head, all.tail: _*)
}
