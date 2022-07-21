// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.Coordinates
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED

import scala.collection.immutable.SortedMap

package object model {
  type DitherNanoMetersRange = Interval.Closed[-1000, 1000]
  type DitherNanoMeters      = BigDecimal Refined DitherNanoMetersRange

  val MaxHourValue = BigDecimal(1000)
  type HourRange = Interval.Closed[0, 1000]
  type Hours     = BigDecimal Refined HourRange
  object Hours extends RefinedTypeOps[Hours, BigDecimal] {
    val Max: Hours = Hours.unsafeFrom(MaxHourValue)
  }

  val NewTargetName: NonEmptyString = "<New Target>"

  val EmptySiderealTarget =
    Target.Sidereal(
      NewTargetName,
      SiderealTracking.const(Coordinates.Zero),
      SourceProfile.Point(
        SpectralDefinition.BandNormalized(
          UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.O5V),
          SortedMap.empty
        )
      ),
      none
    )
}
