// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.ScienceRequirements
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import lucuma.core.math.Wavelength
import lucuma.core.math.Angle
import lucuma.core.math.WavelengthDelta
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.SignalToNoise
import lucuma.core.math.arb.ArbWavelength.given
import lucuma.core.math.arb.ArbAngle.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.math.arb.ArbWavelengthDelta.given
import lucuma.core.math.arb.ArbSignalToNoise.given
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.scalacheck.numeric.given
import lucuma.core.math.arb.ArbRefined.given

trait ArbScienceRequirements:

  given Arbitrary[ScienceRequirements] =
    Arbitrary(
      for
        wavelength         <- arbitrary[Option[Wavelength]]
        resolution         <- arbitrary[Option[PosInt]]
        signalToNoise      <- arbitrary[Option[SignalToNoise]]
        signalToNoiseAt    <- arbitrary[Option[Wavelength]]
        wavelengthCoverage <- arbitrary[Option[WavelengthDelta]]
        focalPlane         <- arbitrary[Option[FocalPlane]]
        focalPlaneAngle    <- arbitrary[Option[Angle]]
        capability         <- arbitrary[Option[SpectroscopyCapabilities]]
      yield ScienceRequirements.Spectroscopy(
        wavelength,
        resolution,
        signalToNoise,
        signalToNoiseAt,
        wavelengthCoverage,
        focalPlane,
        focalPlaneAngle,
        capability
      )
    )

  given Cogen[ScienceRequirements.Spectroscopy] = Cogen[
    (Option[Wavelength],
     Option[PosInt],
     Option[SignalToNoise],
     Option[Wavelength],
     Option[WavelengthDelta],
     Option[FocalPlane],
     Option[Angle],
     Option[SpectroscopyCapabilities]
    )
  ].contramap(sr =>
    (sr.wavelength,
     sr.resolution,
     sr.signalToNoise,
     sr.signalToNoiseAt,
     sr.wavelengthCoverage,
     sr.focalPlane,
     sr.focalPlaneAngle,
     sr.capability
    )
  )

  given Cogen[ScienceRequirements] = Cogen[ScienceRequirements.Spectroscopy].contramap(
    _.asInstanceOf[ScienceRequirements.Spectroscopy]
  )

object ArbScienceRequirements extends ArbScienceRequirements
