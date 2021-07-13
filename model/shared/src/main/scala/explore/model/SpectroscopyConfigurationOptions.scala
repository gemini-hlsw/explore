// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import coulomb.Quantity
import coulomb.cats.implicits._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import explore.model.enum.FocalPlane
import explore.model.enum.SpectroscopyCapabilities
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Micrometer
import monocle.Focus

/**
 * We want to store wavelengths in units to allow 0 in the UI
 * but later we convert these units for calculations were Wavelength 0 is not allowed
 */
final case class SpectroscopyConfigurationOptions(
  wavelengthQ:      Option[Quantity[BigDecimal, Micrometer]],
  resolution:       Option[PosInt],
  signalToNoise:    Option[PosBigDecimal],
  signalToNoiseAtQ: Option[Quantity[BigDecimal, Micrometer]],
  wavelengthRangeQ: Option[Quantity[BigDecimal, Micrometer]],
  focalPlane:       Option[FocalPlane],
  focalPlaneAngle:  Option[Angle],
  capabilities:     Option[SpectroscopyCapabilities]
) {
  def wavelength: Option[Wavelength] =
    wavelengthQ.flatMap(d => Wavelength.decimalMicrometers.getOption(d.value))

  def signalToNoiseAt: Option[Wavelength] =
    signalToNoiseAtQ.flatMap(d => Wavelength.decimalMicrometers.getOption(d.value))

  def wavelengthRange: Option[Wavelength] =
    wavelengthRangeQ.flatMap(d => Wavelength.decimalMicrometers.getOption(d.value))
}

object SpectroscopyConfigurationOptions {
  val wavelengthQ      = Focus[SpectroscopyConfigurationOptions](_.wavelengthQ)
  val resolution       = Focus[SpectroscopyConfigurationOptions](_.resolution)
  val signalToNoise    = Focus[SpectroscopyConfigurationOptions](_.signalToNoise)
  val signalToNoiseAtQ = Focus[SpectroscopyConfigurationOptions](_.signalToNoiseAtQ)
  val wavelengthRangeQ = Focus[SpectroscopyConfigurationOptions](_.wavelengthRangeQ)
  val focalPlane       = Focus[SpectroscopyConfigurationOptions](_.focalPlane)
  val focalPlaneAngle  = Focus[SpectroscopyConfigurationOptions](_.focalPlaneAngle)
  val capabilities     = Focus[SpectroscopyConfigurationOptions](_.capabilities)

  val Default = SpectroscopyConfigurationOptions(None, None, None, None, None, None, None, None)

  implicit val eqSpectroscopyConfigurationOptions: Eq[SpectroscopyConfigurationOptions] = Eq.by(x =>
    (x.wavelengthQ,
     x.resolution,
     x.signalToNoise,
     x.signalToNoiseAtQ,
     x.wavelengthRangeQ,
     x.focalPlane,
     x.focalPlaneAngle,
     x.capabilities
    )
  )
}
