// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.data.NonEmptyList
import lucuma.core.util.Enumerated

/**
 * Enum to indicate a plot's time system
 */
sealed trait TimeDisplay extends Product with Serializable {
  def fold[A](ut: => A, sidereal: => A, local: => A): A =
    this match {
      case TimeDisplay.UT       => ut
      case TimeDisplay.Sidereal => sidereal
      case TimeDisplay.Site     => local
    }

}

object TimeDisplay {
  case object UT       extends TimeDisplay
  case object Sidereal extends TimeDisplay
  case object Site     extends TimeDisplay

  val all = NonEmptyList.of(UT, Sidereal, Site)

  /** @group Typeclass Instances */
  implicit val DisplayEnumerated: Enumerated[TimeDisplay] =
    Enumerated.of(all.head, all.tail: _*)
}
