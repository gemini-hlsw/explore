// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.numeric.Interval

package object model {
  type DitherNanoMetersRange = Interval.Closed[-1000, 1000]
  type DitherNanoMeters      = BigDecimal Refined DitherNanoMetersRange

  val MaxHourValue = BigDecimal(1000)
  type HourRange = Interval.Closed[0, 1000]
  type Hours     = BigDecimal Refined HourRange
  object Hours extends RefinedTypeOps[Hours, BigDecimal] {
    val Max: Hours = Hours.unsafeFrom(MaxHourValue)
  }
}
