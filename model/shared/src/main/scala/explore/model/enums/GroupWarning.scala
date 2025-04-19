// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Order
import cats.derived.*

enum GroupWarning(val shortMsg: String, val longMsg: String) derives Order:
  case BandMismatch
      extends GroupWarning("Mismatched Science Bands",
                           "All observations in an AND group must have the same science band."
      )
  case SiteMismatch
      extends GroupWarning("Mismatched Sites",
                           "All observations in a consecutive AND group must be at the same site."
      )
  case UndefinedObservations
      extends GroupWarning("Undefined Observations", "Group contains undefined observations.")
  case UnapprovedObservations
      extends GroupWarning("Unapproved Observations", "Group contains unapproved observations.")
