// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.syntax.all.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Provides Enumerated and Display instances for Option[A] where A is an enum. This allows, for
 * instance, a dropdown to be used to select an optional enum value where None appears as a `normal`
 * value.
 *
 * @param value
 *   the Enumerated instance for A
 * @param emptyDisplay
 *   the Display instance short and long names for None
 * @param emptyTag
 *   the tag for None - only needed if "__none__" is not a valid tag for A
 */
final case class OptionalEnumerated[A: Enumerated: Display](
  emptyDisplay: String,
  emptyTag:     String = "__none__"
)(using enumerated: Enumerated[A], display: Display[A]):
  given Enumerated[Option[A]] =
    new Enumerated {
      val all                        = None :: enumerated.all.map(Some(_))
      def tag(oa: Option[A]): String = oa match
        case None    => emptyTag
        case Some(a) => enumerated.tag(a)
    }

  given Display[Option[A]] =
    new Display {
      def shortName(oa: Option[A]): String         = oa match
        case None    => emptyDisplay
        case Some(a) => display.shortName(a)
      override def longName(oa: Option[A]): String = oa match
        case None    => emptyDisplay
        case Some(a) => display.longName(a)
    }
