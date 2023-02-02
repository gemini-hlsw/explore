// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import explore.model.ScienceModeAdvanced
import lucuma.core.util.arb.ArbGid.*
import lucuma.core.util.arb.ArbEnumerated.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.arb.ArbExposureTimeMode
import eu.timepit.refined.scalacheck.numeric.*
import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary.*
import explore.model.DitherNanoMeters

trait ArbScienceModeAdvanced {
  import ArbExposureTimeMode.given
  import ArbOffset.*
  import ArbRefined.*
  import ArbWavelength.*

  given Arbitrary[ScienceModeAdvanced.GmosNorthLongSlit] =
    Arbitrary[ScienceModeAdvanced.GmosNorthLongSlit](
      for {
        overrideWavelength        <- arbitrary[Option[Wavelength]]
        overrideGrating           <- arbitrary[Option[GmosNorthGrating]]
        overrideFilter            <- arbitrary[Option[GmosNorthFilter]]
        overrideFpu               <- arbitrary[Option[GmosNorthFpu]]
        overrideExposureTimeMode  <- arbitrary[Option[ExposureTimeMode]]
        explicitXBin              <- arbitrary[Option[GmosXBinning]]
        explicitYBin              <- arbitrary[Option[GmosYBinning]]
        explicitAmpReadMode       <- arbitrary[Option[GmosAmpReadMode]]
        explicitAmpGain           <- arbitrary[Option[GmosAmpGain]]
        explicitRoi               <- arbitrary[Option[GmosRoi]]
        explicitWavelengthDithers <- arbitrary[Option[NonEmptyList[DitherNanoMeters]]]
        explicitSpatialOffsets    <- arbitrary[Option[NonEmptyList[Offset.Q]]]
      } yield ScienceModeAdvanced.GmosNorthLongSlit(
        overrideWavelength,
        overrideGrating,
        overrideFilter,
        overrideFpu,
        overrideExposureTimeMode,
        explicitXBin,
        explicitYBin,
        explicitAmpReadMode,
        explicitAmpGain,
        explicitRoi,
        explicitWavelengthDithers,
        explicitSpatialOffsets
      )
    )

  given Arbitrary[ScienceModeAdvanced.GmosSouthLongSlit] =
    Arbitrary[ScienceModeAdvanced.GmosSouthLongSlit](
      for {
        overrideWavelength        <- arbitrary[Option[Wavelength]]
        overrideGrating           <- arbitrary[Option[GmosSouthGrating]]
        overrideFilter            <- arbitrary[Option[GmosSouthFilter]]
        overrideFpu               <- arbitrary[Option[GmosSouthFpu]]
        overrideExposureTimeMode  <- arbitrary[Option[ExposureTimeMode]]
        explicitXBin              <- arbitrary[Option[GmosXBinning]]
        explicitYBin              <- arbitrary[Option[GmosYBinning]]
        explicitAmpReadMode       <- arbitrary[Option[GmosAmpReadMode]]
        explicitAmpGain           <- arbitrary[Option[GmosAmpGain]]
        explicitRoi               <- arbitrary[Option[GmosRoi]]
        explicitWavelengthDithers <- arbitrary[Option[NonEmptyList[DitherNanoMeters]]]
        explicitSpatialOffsets    <- arbitrary[Option[NonEmptyList[Offset.Q]]]
      } yield ScienceModeAdvanced.GmosSouthLongSlit(
        overrideWavelength,
        overrideGrating,
        overrideFilter,
        overrideFpu,
        overrideExposureTimeMode,
        explicitXBin,
        explicitYBin,
        explicitAmpReadMode,
        explicitAmpGain,
        explicitRoi,
        explicitWavelengthDithers,
        explicitSpatialOffsets
      )
    )

  given Arbitrary[ScienceModeAdvanced] =
    Arbitrary[ScienceModeAdvanced](
      Gen.oneOf(
        arbitrary[ScienceModeAdvanced.GmosNorthLongSlit],
        arbitrary[ScienceModeAdvanced.GmosSouthLongSlit]
      )
    )

  given Cogen[ScienceModeAdvanced.GmosNorthLongSlit] =
    Cogen[
      (Option[Wavelength],
       Option[GmosNorthGrating],
       Option[GmosNorthFilter],
       Option[GmosNorthFpu],
       Option[ExposureTimeMode],
       Option[GmosXBinning],
       Option[GmosYBinning],
       Option[GmosAmpReadMode],
       Option[GmosAmpGain],
       Option[GmosRoi],
       Option[NonEmptyList[DitherNanoMeters]],
       Option[NonEmptyList[Offset.Q]]
      )
    ]
      .contramap(o =>
        (o.overrideWavelength,
         o.overrideGrating,
         o.overrideFilter,
         o.overrideFpu,
         o.overrideExposureTimeMode,
         o.explicitXBin,
         o.explicitYBin,
         o.explicitAmpReadMode,
         o.explicitAmpGain,
         o.explicitRoi,
         o.explicitWavelengthDithers,
         o.explicitSpatialOffsets
        )
      )

  given Cogen[ScienceModeAdvanced.GmosSouthLongSlit] =
    Cogen[
      (Option[Wavelength],
       Option[GmosSouthGrating],
       Option[GmosSouthFilter],
       Option[GmosSouthFpu],
       Option[ExposureTimeMode],
       Option[GmosXBinning],
       Option[GmosYBinning],
       Option[GmosAmpReadMode],
       Option[GmosAmpGain],
       Option[GmosRoi],
       Option[NonEmptyList[DitherNanoMeters]],
       Option[NonEmptyList[Offset.Q]]
      )
    ]
      .contramap(o =>
        (o.overrideWavelength,
         o.overrideGrating,
         o.overrideFilter,
         o.overrideFpu,
         o.overrideExposureTimeMode,
         o.explicitXBin,
         o.explicitYBin,
         o.explicitAmpReadMode,
         o.explicitAmpGain,
         o.explicitRoi,
         o.explicitWavelengthDithers,
         o.explicitSpatialOffsets
        )
      )

  given Cogen[ScienceModeAdvanced] =
    Cogen[Either[ScienceModeAdvanced.GmosNorthLongSlit, ScienceModeAdvanced.GmosSouthLongSlit]]
      .contramap {
        case n @ ScienceModeAdvanced.GmosNorthLongSlit(_, _, _, _, _, _, _, _, _, _, _, _) =>
          n.asLeft
        case s @ ScienceModeAdvanced.GmosSouthLongSlit(_, _, _, _, _, _, _, _, _, _, _, _) =>
          s.asRight
      }

}

object ArbScienceModeAdvanced extends ArbScienceModeAdvanced
