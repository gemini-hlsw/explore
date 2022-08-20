// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

/**
 * Enum to indicate a plot's time system
 */
enum TimeDisplay(val tag: String):
  case UT       extends TimeDisplay("ut")
  case Sidereal extends TimeDisplay("sidereal")
  case Site     extends TimeDisplay("site")

  def fold[A](ut: => A, sidereal: => A, local: => A): A =
    this match {
      case TimeDisplay.UT       => ut
      case TimeDisplay.Sidereal => sidereal
      case TimeDisplay.Site     => local
    }

object TimeDisplay:

  /** @group Typeclass Instances */
  given Enumerated[TimeDisplay] =
    Enumerated.from(UT, Sidereal, Site).withTag(_.tag)
