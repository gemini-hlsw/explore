// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.data.NonEmptyList
import lucuma.core.util.Enumerated
import monocle.Iso

/**
 * Enum to indicate visibility of an item, roughly equivalent to css display
 */
enum Visible:
  case Hidden, Inline

  def fold[A](hidden: => A, inline: => A): A =
    this match {
      case Visible.Hidden => hidden
      case Visible.Inline => inline
    }

  def visible: Boolean = fold(false, true)

  def flip: Visible = fold(Visible.Inline, Visible.Hidden)

object Visible:

  val boolIso: Iso[Boolean, Visible] = Iso[Boolean, Visible] { b =>
    if (b) Inline else Hidden
  } {
    case Hidden => false
    case Inline => true
  }

  /** @group Typeclass Instances */
  given Enumerated[Visible] =
    Enumerated.from(Hidden, Inline).withTag(_.visible.toString)
