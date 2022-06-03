// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import cats.data.NonEmptyList
import lucuma.core.util.Enumerated

/**
 * Enum to indicate if a plot has a Night/Semester range
 */
sealed trait PlotRange extends Product with Serializable {
  def fold[A](night: => A, semester: => A): A =
    this match {
      case PlotRange.Night    => night
      case PlotRange.Semester => semester
    }

  def flip: PlotRange = fold(PlotRange.Semester, PlotRange.Night)
}

object PlotRange {
  case object Night    extends PlotRange
  case object Semester extends PlotRange

  val all = NonEmptyList.of(Night, Semester)

  /** @group Typeclass Instances */
  implicit val DisplayEnumerated: Enumerated[PlotRange] =
    Enumerated.of(all.head, all.tail: _*)
}
