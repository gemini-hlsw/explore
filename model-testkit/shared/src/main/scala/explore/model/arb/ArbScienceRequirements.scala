// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.scalacheck.numeric.given
import explore.model.ScienceRequirements
import explore.model.ScienceRequirements.*
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.Angle
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.arb.ArbWavelength.given
import lucuma.core.math.arb.ArbAngle.given
import lucuma.core.math.arb.ArbWavelengthDelta.given
import lucuma.core.math.arb.ArbSignalToNoise.given
import lucuma.core.math.arb.ArbRefined.given
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbTimeSpan.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbScienceRequirements:

  given Arbitrary[SignalToNoiseModeInfo] =
    Arbitrary(
      for
        value <- arbitrary[Option[SignalToNoise]]
        at    <- arbitrary[Option[Wavelength]]
      yield SignalToNoiseModeInfo(value, at)
    )

  given Cogen[SignalToNoiseModeInfo] =
    Cogen[(Option[SignalToNoise], Option[Wavelength])].contramap(snmi => (snmi.value, snmi.at))

  given Arbitrary[TimeAndCountModeInfo] =
    Arbitrary(
      for
        time  <- arbitrary[Option[TimeSpan]]
        count <- arbitrary[Option[NonNegInt]]
        at    <- arbitrary[Option[Wavelength]]
      yield TimeAndCountModeInfo(time, count, at)
    )

  given Cogen[TimeAndCountModeInfo] =
    Cogen[(Option[TimeSpan], Option[NonNegInt], Option[Wavelength])].contramap(tcmi =>
      (tcmi.time, tcmi.count, tcmi.at)
    )

  given Arbitrary[ExposureTimeModeInfo] =
    Arbitrary(
      for mode <- arbitrary[Either[SignalToNoiseModeInfo, TimeAndCountModeInfo]]
      yield ExposureTimeModeInfo(mode)
    )

  given Cogen[ExposureTimeModeInfo] =
    Cogen[Either[SignalToNoiseModeInfo, TimeAndCountModeInfo]].contramap(
      _.mode
    )

  given Arbitrary[ScienceRequirements] =
    Arbitrary(
      for
        wavelength         <- arbitrary[Option[Wavelength]]
        resolution         <- arbitrary[Option[PosInt]]
        exposureTimeMode   <- arbitrary[ExposureTimeModeInfo]
        wavelengthCoverage <- arbitrary[Option[WavelengthDelta]]
        focalPlane         <- arbitrary[Option[FocalPlane]]
        focalPlaneAngle    <- arbitrary[Option[Angle]]
        capability         <- arbitrary[Option[SpectroscopyCapabilities]]
      yield ScienceRequirements.Spectroscopy(
        wavelength,
        resolution,
        exposureTimeMode,
        wavelengthCoverage,
        focalPlane,
        focalPlaneAngle,
        capability
      )
    )

  given Cogen[ScienceRequirements.Spectroscopy] = Cogen[
    (Option[Wavelength],
     Option[PosInt],
     ExposureTimeModeInfo,
     Option[WavelengthDelta],
     Option[FocalPlane],
     Option[Angle],
     Option[SpectroscopyCapabilities]
    )
  ].contramap(sr =>
    (sr.wavelength,
     sr.resolution,
     sr.exposureTimeMode,
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
