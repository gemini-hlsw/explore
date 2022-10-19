// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

/**
 * Enum to indicate the direction a column in sorted
 */
enum SortDirection(val tag: String) derives Enumerated:
  case Asc  extends SortDirection("asc")
  case Desc extends SortDirection("desc")

object SortDirection:
  def fromDescending(b: Boolean) = if (b) SortDirection.Desc else SortDirection.Asc

  extension (d: SortDirection)
    def toDescending: Boolean = d match
      case Asc  => false
      case Desc => true
