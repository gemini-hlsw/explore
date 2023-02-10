// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import cats.syntax.all.*
import coulomb.*
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.constants.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Band
import lucuma.core.math.ApparentRadialVelocity
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Constants.*
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Redshift
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.units.*
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.optics.SplitEpi
import lucuma.core.syntax.time.*
import lucuma.core.util.TimeSpan
import monocle.Getter
import monocle.Optional
import monocle.*

import java.time.Duration
import scala.collection.immutable.SortedMap

/**
 * Contains a set of useful optics to explore the model
 */
trait ModelOptics {

  val fromKilometersPerSecondCZ: Iso[BigDecimal, ApparentRadialVelocity] =
    Iso[BigDecimal, ApparentRadialVelocity](b =>
      ApparentRadialVelocity(b.withUnit[KilometersPerSecond].toUnit[MetersPerSecond])
    )(cz => cz.cz.toUnit[KilometersPerSecond].value)

  val redshiftBigDecimalIso: Iso[BigDecimal, Redshift] = Iso(Redshift.apply)(_.z)

  val fromKilometersPerSecondRV: Prism[BigDecimal, RadialVelocity] =
    Prism[BigDecimal, RadialVelocity](b =>
      Some(b)
        .filter(_.abs <= SpeedOfLight.tToUnit[KilometersPerSecond].value)
        .flatMap(v => RadialVelocity(v.withUnit[KilometersPerSecond]))
    )(rv => rv.rv.toUnit[KilometersPerSecond].value)

  /** Direct optic into a defined RadialVelocity in a SiderealTarget */
  val targetRV: Optional[Target, RadialVelocity] =
    Target.sidereal.andThen(Target.Sidereal.radialVelocity.some)

  /**
   * Getter for any kind of brightness measures of a `Target`, as long as it has a
   * `SpectralDefinition.BandNormalized`
   */
  val targetBrightnesses: Getter[Target, Option[SortedMap[Band, Measure[BrightnessValue]]]] =
    Getter { target =>
      val sourceProfile = Target.sourceProfile.get(target)
      SourceProfile.integratedBrightnesses
        .getOption(sourceProfile)
        .orElse(SourceProfile.surfaceBrightnesses.getOption(sourceProfile))
    }

  val optionNonEmptyStringIso: Iso[Option[NonEmptyString], String] =
    Iso[Option[NonEmptyString], String](_.foldMap(_.value))(s => NonEmptyString.from(s).toOption)

  // Note: truncates to Int.MaxValue - shouldn't have durations longer than that...
  val timeSpanSecondsSplitEpi: SplitEpi[TimeSpan, NonNegInt] = SplitEpi(
    ts => NonNegInt.unsafeFrom(math.min(ts.toSeconds.longValue, Int.MaxValue.toLong).toInt),
    secs => TimeSpan.unsafeFromDuration(secs.value.toLong.seconds)
  )
}
