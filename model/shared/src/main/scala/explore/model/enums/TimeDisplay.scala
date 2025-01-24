// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enum to indicate a plot's time system
 */
enum TimeDisplay(val tag: String) derives Enumerated:
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

  given Display[TimeDisplay] = Display.byShortName {
    case TimeDisplay.UT => TimeDisplay.UT.tag.toUpperCase
    case e              => e.tag.capitalize
  }
