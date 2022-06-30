// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.syntax.all._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import explore.model.ScienceModeAdvanced
import lucuma.core.util.arb.ArbGid._
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.enums._
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.arb.ArbRefined
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.arb.ArbExposureTimeMode
import eu.timepit.refined.scalacheck.numeric._
import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary._
import explore.model.DitherNanoMeters

trait ArbScienceModeAdvanced {
  import ArbExposureTimeMode._
  import ArbOffset._
  import ArbRefined._

  implicit val arbGmosNorthLongSlitAdvanced = Arbitrary[ScienceModeAdvanced.GmosNorthLongSlit](
    for {
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

  implicit val arbGmosSouthLongSlitAdvanced = Arbitrary[ScienceModeAdvanced.GmosSouthLongSlit](
    for {
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

  implicit val arbScienceModeAdvanced: Arbitrary[ScienceModeAdvanced] =
    Arbitrary[ScienceModeAdvanced](
      Gen.oneOf(
        arbitrary[ScienceModeAdvanced.GmosNorthLongSlit],
        arbitrary[ScienceModeAdvanced.GmosSouthLongSlit]
      )
    )

  implicit val cogenGmosNorthLongSlitAdvanced: Cogen[ScienceModeAdvanced.GmosNorthLongSlit] =
    Cogen[
      (Option[GmosNorthGrating],
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
        (o.overrideGrating,
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

  implicit val cogenGmosSouthLongSlitAdvanced: Cogen[ScienceModeAdvanced.GmosSouthLongSlit] =
    Cogen[
      (Option[GmosSouthGrating],
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
        (o.overrideGrating,
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

  implicit val cogenScienceModeAdvanced: Cogen[ScienceModeAdvanced] =
    Cogen[Either[ScienceModeAdvanced.GmosNorthLongSlit, ScienceModeAdvanced.GmosSouthLongSlit]]
      .contramap {
        case n @ ScienceModeAdvanced.GmosNorthLongSlit(_, _, _, _, _, _, _, _, _, _, _) => n.asLeft
        case s @ ScienceModeAdvanced.GmosSouthLongSlit(_, _, _, _, _, _, _, _, _, _, _) => s.asRight
      }

}

object ArbScienceModeAdvanced extends ArbScienceModeAdvanced
