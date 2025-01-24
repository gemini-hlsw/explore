// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enum to indicate if a plot has a Night/Semester range
 */
enum PlotRange(val tag: String) derives Enumerated:
  case Night    extends PlotRange("night")
  case FullDay  extends PlotRange("fullday")
  case Semester extends PlotRange("semester")

  def fold[A](night: => A, fullDay: => A, semester: => A): A =
    this match {
      case PlotRange.Night    => night
      case PlotRange.FullDay  => fullDay
      case PlotRange.Semester => semester
    }

object PlotRange:
  given Display[PlotRange] = Display.byShortName(_.fold("Night", "24h", "Semester"))
