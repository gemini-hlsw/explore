// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import coulomb._
import coulomb.accepted.Percent
import coulomb.time.Hour
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.numeric.NonNegDouble

package object refined {
  type NonNegHour = Quantity[NonNegDouble, Hour]

  type ZeroTo100    = Interval.Closed[0, 100]
  type IntZeroTo100 = Int Refined ZeroTo100
  type IntPercent   = Quantity[IntZeroTo100, Percent]

}
