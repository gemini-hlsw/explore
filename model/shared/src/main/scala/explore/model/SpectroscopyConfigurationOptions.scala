// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import coulomb.Quantity
import coulomb.syntax.*
import coulomb.cats.quantity.ctx_Quantity_Eq
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Micrometer
import monocle.Focus

/**
 * We want to store wavelengths in units to allow 0 in the UI but later we convert these units for
 * calculations were Wavelength 0 is not allowed
 */
final case class SpectroscopyConfigurationOptions(
  wavelengthQ:         Option[Quantity[BigDecimal, Micrometer]],
  resolution:          Option[PosInt],
  signalToNoise:       Option[PosBigDecimal],
  signalToNoiseAtQ:    Option[Quantity[BigDecimal, Micrometer]],
  wavelengthCoverageQ: Option[Quantity[BigDecimal, Micrometer]],
  focalPlane:          Option[FocalPlane],
  focalPlaneAngle:     Option[Angle],
  capabilities:        Option[SpectroscopyCapabilities]
) {
  def wavelength: Option[Wavelength] =
    wavelengthQ.flatMap(d => Wavelength.decimalMicrometers.getOption(d.value))

  def signalToNoiseAt: Option[Wavelength] =
    signalToNoiseAtQ.flatMap(d => Wavelength.decimalMicrometers.getOption(d.value))

  def wavelengthCoverage: Option[Wavelength] =
    wavelengthCoverageQ.flatMap(d => Wavelength.decimalMicrometers.getOption(d.value))
}

object SpectroscopyConfigurationOptions {
  val wavelengthQ         = Focus[SpectroscopyConfigurationOptions](_.wavelengthQ)
  val resolution          = Focus[SpectroscopyConfigurationOptions](_.resolution)
  val signalToNoise       = Focus[SpectroscopyConfigurationOptions](_.signalToNoise)
  val signalToNoiseAtQ    = Focus[SpectroscopyConfigurationOptions](_.signalToNoiseAtQ)
  val wavelengthCoverageQ = Focus[SpectroscopyConfigurationOptions](_.wavelengthCoverageQ)
  val focalPlane          = Focus[SpectroscopyConfigurationOptions](_.focalPlane)
  val focalPlaneAngle     = Focus[SpectroscopyConfigurationOptions](_.focalPlaneAngle)
  val capabilities        = Focus[SpectroscopyConfigurationOptions](_.capabilities)

  val Default = SpectroscopyConfigurationOptions(None, None, None, None, None, None, None, None)

  implicit val eqSpectroscopyConfigurationOptions: Eq[SpectroscopyConfigurationOptions] = Eq.by(x =>
    (x.wavelengthQ,
     x.resolution,
     x.signalToNoise,
     x.signalToNoiseAtQ,
     x.wavelengthCoverageQ,
     x.focalPlane,
     x.focalPlaneAngle,
     x.capabilities
    )
  )
}
