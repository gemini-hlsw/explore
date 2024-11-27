// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import cats.Order.given
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import lucuma.core.enums.Band
import lucuma.core.math.ApparentRadialVelocity
import lucuma.core.math.BrightnessUnits.LineWidthQuantity
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Constants.*
import lucuma.core.math.LineFluxValue
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.units.*
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import monocle.*
import monocle.Getter
import monocle.Optional

import scala.collection.immutable.SortedMap

/**
 * Contains a set of useful optics to explore the model
 */
trait ModelOptics {

  val FromKilometersPerSecondCZ: Iso[BigDecimal, ApparentRadialVelocity] =
    Iso[BigDecimal, ApparentRadialVelocity](b =>
      ApparentRadialVelocity(b.withUnit[KilometersPerSecond].toUnit[MetersPerSecond])
    )(cz => cz.cz.toUnit[KilometersPerSecond].value)

  val FromKilometersPerSecondRV: Prism[BigDecimal, RadialVelocity] =
    Prism[BigDecimal, RadialVelocity](b =>
      Some(b)
        .filter(_.abs <= SpeedOfLight.tToUnit[KilometersPerSecond].value)
        .flatMap(v => RadialVelocity(v.withUnit[KilometersPerSecond]))
    )(rv => rv.rv.toUnit[KilometersPerSecond].value)

  /** Direct optic into a defined RadialVelocity in a SiderealTarget */
  val TargetRV: Optional[Target, RadialVelocity] =
    Target.sidereal.andThen(Target.Sidereal.radialVelocity.some)

  /**
   * Getter for any kind of brightness measures of a `SourceProfile`, as long as it has a
   * `SpectralDefinition.BandNormalized`
   */
  val BandNormalizedBrightnesses
    : Getter[SourceProfile, Option[SortedMap[Band, Measure[BrightnessValue]]]] =
    Getter: sourceProfile =>
      SourceProfile.integratedBrightnesses
        .getOption(sourceProfile)
        .orElse(SourceProfile.surfaceBrightnesses.getOption(sourceProfile))

  /**
   * Getter for any kind of brightness measures of a `Target`, as long as it has a
   * `SpectralDefinition.BandNormalized`
   */
  val BandNormalizedTargetBrightnesses
    : Getter[Target, Option[SortedMap[Band, Measure[BrightnessValue]]]] =
    Target.sourceProfile.asGetter.andThen(BandNormalizedBrightnesses)

  /**
   * Getter for any kind of brightness measures of a `SourceProfile`, as long as it has a
   * `SpectralDefinition.EmissionLines`
   */
  val EmissionLinesBrightnesses: Getter[SourceProfile, Option[
    SortedMap[Wavelength, (LineWidthQuantity, Measure[LineFluxValue])]
  ]] =
    Getter: sourceProfile =>
      SourceProfile.integratedWavelengthLines
        .getOption(sourceProfile)
        .orElse(SourceProfile.surfaceWavelengthLines.getOption(sourceProfile))
        .map: m =>
          SortedMap.from:
            m.view
              .mapValues[(LineWidthQuantity, Measure[LineFluxValue])]: l =>
                (l.lineWidth, l.lineFlux)
}
