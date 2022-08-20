// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

/**
 * Enum to indicate if a plot has a Night/Semester range
 */
enum PlotRange(val tag: String):
  case Night    extends PlotRange("night")
  case Semester extends PlotRange("semester")

  def fold[A](night: => A, semester: => A): A =
    this match {
      case PlotRange.Night    => night
      case PlotRange.Semester => semester
    }

  def flip: PlotRange = fold(PlotRange.Semester, PlotRange.Night)

object PlotRange:

  /** @group Typeclass Instances */
  given Enumerated[PlotRange] =
    Enumerated.from(Night, Semester).withTag(_.tag)
