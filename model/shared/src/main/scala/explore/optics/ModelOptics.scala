// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import cats.syntax.all._
import coulomb._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Band
import lucuma.core.math.ApparentRadialVelocity
import lucuma.core.math.Constants._
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Redshift
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.units._
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import monocle.Getter
import monocle.Optional
import monocle._

import scala.collection.immutable.SortedMap

/**
 * Contains a set of useful optics to explore the model
 */
trait ModelOptics {

  val fromKilometersPerSecondCZ: Iso[BigDecimal, ApparentRadialVelocity] =
    Iso[BigDecimal, ApparentRadialVelocity](b =>
      ApparentRadialVelocity(b.withUnit[KilometersPerSecond])
    )(cz => cz.cz.toUnit[KilometersPerSecond].value)

  val redshiftBigDecimalIso: Iso[BigDecimal, Redshift] = Iso(Redshift.apply)(_.z)

  val fromKilometersPerSecondRV: Prism[BigDecimal, RadialVelocity] =
    Prism[BigDecimal, RadialVelocity](b =>
      Some(b)
        .filter(_.abs <= SpeedOfLight.to[BigDecimal, KilometersPerSecond].value)
        .flatMap(v => RadialVelocity(v.withUnit[KilometersPerSecond]))
    )(rv => rv.rv.toUnit[KilometersPerSecond].value)

  /** Direct optic into a defined RadialVelocity in a SiderealTarget */
  val targetRV: Optional[Target, RadialVelocity] =
    Target.sidereal.andThen(Target.Sidereal.radialVelocity.some)

  /**
   * Getter for any kind of brightness measures of a `Target`, as long as it has a
   * `SpectralDefinition.BandNormalized`
   */
  val targetBrightnesses: Getter[Target, Option[SortedMap[Band, Measure[BigDecimal]]]] =
    Getter { target =>
      val sourceProfile = Target.sourceProfile.get(target)
      SourceProfile.integratedBrightnesses
        .getOption(sourceProfile)
        .orElse(SourceProfile.surfaceBrightnesses.getOption(sourceProfile))
    }

  val optionNonEmptyStringIso: Iso[Option[NonEmptyString], String] =
    Iso[Option[NonEmptyString], String](_.foldMap(_.value))(s => NonEmptyString.from(s).toOption)
}

object ModelOptics extends ModelOptics
