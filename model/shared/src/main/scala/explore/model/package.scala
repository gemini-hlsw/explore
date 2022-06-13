// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval

package object model {
  protected val Minus1000 = BigDecimal(-1000)
  protected val Plus1000  = BigDecimal(1000)
  type DitherNanoMetersRange = Interval.Closed[Minus1000.type, Plus1000.type]
  type DitherNanoMeters      = BigDecimal Refined DitherNanoMetersRange
}
