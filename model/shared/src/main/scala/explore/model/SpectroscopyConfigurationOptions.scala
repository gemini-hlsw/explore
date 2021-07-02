// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import explore.model.enum.SpectroscopyCapabilities
import explore.modes.FocalPlane
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import monocle.macros.Lenses

@Lenses
final case class SpectroscopyConfigurationOptions(
  wavelength:      Option[Wavelength],
  resolution:      Option[PosInt],
  signalToNoise:   Option[PosBigDecimal],
  signalToNoiseAt: Option[Wavelength],
  wavelengthRange: Option[Wavelength],
  focalPlane:      Option[FocalPlane],
  focalPlaneAngle: Option[Angle],
  capabilities:    Option[SpectroscopyCapabilities]
)

object SpectroscopyConfigurationOptions {
  val Default = SpectroscopyConfigurationOptions(None, None, None, None, None, None, None, None)

  implicit val eqSpectroscopyConfigurationOptions: Eq[SpectroscopyConfigurationOptions] = Eq.by(x =>
    (x.wavelength,
     x.resolution,
     x.signalToNoise,
     x.signalToNoiseAt,
     x.wavelengthRange,
     x.focalPlane,
     x.focalPlaneAngle,
     x.capabilities
    )
  )
}
