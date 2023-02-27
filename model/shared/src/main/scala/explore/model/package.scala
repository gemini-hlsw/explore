// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.auto.*
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.Coordinates
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED
import lucuma.core.util.NewType
import lucuma.refined.*

import scala.collection.immutable.SortedMap

val MaxHourValue = BigDecimal(1000)
type HourRange = Interval.Closed[0, 1000]
type Hours     = BigDecimal Refined HourRange
object Hours extends RefinedTypeOps[Hours, BigDecimal] {
  val Max: Hours = Hours.unsafeFrom(MaxHourValue)
}

val NewTargetName: NonEmptyString = "<New Target>".refined

val EmptySiderealTarget =
  Target.Sidereal(
    NewTargetName,
    SiderealTracking.const(Coordinates.Zero),
    SourceProfile.Point(SpectralDefinition.BandNormalized(none, SortedMap.empty)),
    none
  )
