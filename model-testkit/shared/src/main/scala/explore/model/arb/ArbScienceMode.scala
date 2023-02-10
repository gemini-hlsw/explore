// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary.*
import cats.syntax.all.*
import eu.timepit.refined.scalacheck.numeric.*
import explore.model.ScienceMode
import explore.model.itc.CoverageCenterWavelength
import lucuma.core.util.arb.ArbGid.*
import lucuma.core.util.arb.ArbEnumerated.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.arb.ArbWavelengthDither
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.arb.ArbExposureTimeMode
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbScienceMode {
  import ArbExposureTimeMode.given
  import ArbOffset.*
  import ArbRefined.*
  import ArbWavelength.*
  import ArbWavelengthDither.given

  given Arbitrary[ScienceMode.GmosNorthLongSlit] =
    Arbitrary[ScienceMode.GmosNorthLongSlit](
      for {
        initialGrating            <- arbitrary[GmosNorthGrating]
        grating                   <- arbitrary[GmosNorthGrating]
        initialFilter             <- arbitrary[Option[GmosNorthFilter]]
        filter                    <- arbitrary[Option[GmosNorthFilter]]
        initialFpu                <- arbitrary[GmosNorthFpu]
        fpu                       <- arbitrary[GmosNorthFpu]
        initialCentralWavelength  <- arbitrary[Wavelength]
        centralWavelength         <- arbitrary[Wavelength]
        defaultXBin               <- arbitrary[GmosXBinning]
        explicitXBin              <- arbitrary[Option[GmosXBinning]]
        defaultYBin               <- arbitrary[GmosYBinning]
        explicitYBin              <- arbitrary[Option[GmosYBinning]]
        defaultAmpReadMode        <- arbitrary[GmosAmpReadMode]
        explicitAmpReadMode       <- arbitrary[Option[GmosAmpReadMode]]
        defaultAmpGain            <- arbitrary[GmosAmpGain]
        explicitAmpGain           <- arbitrary[Option[GmosAmpGain]]
        defaultRoi                <- arbitrary[GmosRoi]
        explicitRoi               <- arbitrary[Option[GmosRoi]]
        defaultWavelengthDithers  <- arbitrary[NonEmptyList[WavelengthDither]]
        explicitWavelengthDithers <- arbitrary[Option[NonEmptyList[WavelengthDither]]]
        defaultSpatialOffsets     <- arbitrary[NonEmptyList[Offset.Q]]
        explicitSpatialOffsets    <- arbitrary[Option[NonEmptyList[Offset.Q]]]
      } yield ScienceMode.GmosNorthLongSlit(
        initialGrating,
        grating,
        initialFilter,
        filter,
        initialFpu,
        fpu,
        CoverageCenterWavelength(initialCentralWavelength),
        CoverageCenterWavelength(centralWavelength),
        defaultXBin,
        explicitXBin,
        defaultYBin,
        explicitYBin,
        defaultAmpReadMode,
        explicitAmpReadMode,
        defaultAmpGain,
        explicitAmpGain,
        defaultRoi,
        explicitRoi,
        defaultWavelengthDithers,
        explicitWavelengthDithers,
        defaultSpatialOffsets,
        explicitSpatialOffsets
      )
    )

  given Arbitrary[ScienceMode.GmosSouthLongSlit] =
    Arbitrary[ScienceMode.GmosSouthLongSlit](
      for {
        initialGrating            <- arbitrary[GmosSouthGrating]
        grating                   <- arbitrary[GmosSouthGrating]
        initialFilter             <- arbitrary[Option[GmosSouthFilter]]
        filter                    <- arbitrary[Option[GmosSouthFilter]]
        initialFpu                <- arbitrary[GmosSouthFpu]
        fpu                       <- arbitrary[GmosSouthFpu]
        initialCentralWavelength  <- arbitrary[Wavelength]
        centralWavelength         <- arbitrary[Wavelength]
        defaultXBin               <- arbitrary[GmosXBinning]
        explicitXBin              <- arbitrary[Option[GmosXBinning]]
        defaultYBin               <- arbitrary[GmosYBinning]
        explicitYBin              <- arbitrary[Option[GmosYBinning]]
        defaultAmpReadMode        <- arbitrary[GmosAmpReadMode]
        explicitAmpReadMode       <- arbitrary[Option[GmosAmpReadMode]]
        defaultAmpGain            <- arbitrary[GmosAmpGain]
        explicitAmpGain           <- arbitrary[Option[GmosAmpGain]]
        defaultRoi                <- arbitrary[GmosRoi]
        explicitRoi               <- arbitrary[Option[GmosRoi]]
        defaultWavelengthDithers  <- arbitrary[NonEmptyList[WavelengthDither]]
        explicitWavelengthDithers <- arbitrary[Option[NonEmptyList[WavelengthDither]]]
        defaultSpatialOffsets     <- arbitrary[NonEmptyList[Offset.Q]]
        explicitSpatialOffsets    <- arbitrary[Option[NonEmptyList[Offset.Q]]]
      } yield ScienceMode.GmosSouthLongSlit(
        initialGrating,
        grating,
        initialFilter,
        filter,
        initialFpu,
        fpu,
        CoverageCenterWavelength(initialCentralWavelength),
        CoverageCenterWavelength(centralWavelength),
        defaultXBin,
        explicitXBin,
        defaultYBin,
        explicitYBin,
        defaultAmpReadMode,
        explicitAmpReadMode,
        defaultAmpGain,
        explicitAmpGain,
        defaultRoi,
        explicitRoi,
        defaultWavelengthDithers,
        explicitWavelengthDithers,
        defaultSpatialOffsets,
        explicitSpatialOffsets
      )
    )

  given Arbitrary[ScienceMode] = Arbitrary[ScienceMode](
    Gen.oneOf(
      arbitrary[ScienceMode.GmosNorthLongSlit],
      arbitrary[ScienceMode.GmosSouthLongSlit]
    )
  )

  given Cogen[ScienceMode.GmosNorthLongSlit] =
    Cogen[
      (GmosNorthGrating,
       GmosNorthGrating,
       Option[GmosNorthFilter],
       Option[GmosNorthFilter],
       GmosNorthFpu,
       GmosNorthFpu,
       Wavelength,
       Wavelength,
       GmosXBinning,
       Option[GmosXBinning],
       GmosYBinning,
       Option[GmosYBinning],
       GmosAmpReadMode,
       Option[GmosAmpReadMode],
       GmosAmpGain,
       Option[GmosAmpGain],
       GmosRoi,
       Option[GmosRoi],
       NonEmptyList[WavelengthDither],
       Option[NonEmptyList[WavelengthDither]],
       NonEmptyList[Offset.Q],
       Option[NonEmptyList[Offset.Q]]
      )
    ]
      .contramap(o =>
        (o.initialGrating,
         o.grating,
         o.initialFilter,
         o.filter,
         o.initialFpu,
         o.fpu,
         o.initialCentralWavelength.value,
         o.centralWavelength.value,
         o.defaultXBin,
         o.explicitXBin,
         o.defaultYBin,
         o.explicitYBin,
         o.defaultAmpReadMode,
         o.explicitAmpReadMode,
         o.defaultAmpGain,
         o.explicitAmpGain,
         o.defaultRoi,
         o.explicitRoi,
         o.defaultWavelengthDithers,
         o.explicitWavelengthDithers,
         o.defaultSpatialOffsets,
         o.explicitSpatialOffsets
        )
      )

  given Cogen[ScienceMode.GmosSouthLongSlit] =
    Cogen[
      (GmosSouthGrating,
       GmosSouthGrating,
       Option[GmosSouthFilter],
       Option[GmosSouthFilter],
       GmosSouthFpu,
       GmosSouthFpu,
       Wavelength,
       Wavelength,
       GmosXBinning,
       Option[GmosXBinning],
       GmosYBinning,
       Option[GmosYBinning],
       GmosAmpReadMode,
       Option[GmosAmpReadMode],
       GmosAmpGain,
       Option[GmosAmpGain],
       GmosRoi,
       Option[GmosRoi],
       NonEmptyList[WavelengthDither],
       Option[NonEmptyList[WavelengthDither]],
       NonEmptyList[Offset.Q],
       Option[NonEmptyList[Offset.Q]]
      )
    ]
      .contramap(o =>
        (o.initialGrating,
         o.grating,
         o.initialFilter,
         o.filter,
         o.initialFpu,
         o.fpu,
         o.initialCentralWavelength.value,
         o.centralWavelength.value,
         o.defaultXBin,
         o.explicitXBin,
         o.defaultYBin,
         o.explicitYBin,
         o.defaultAmpReadMode,
         o.explicitAmpReadMode,
         o.defaultAmpGain,
         o.explicitAmpGain,
         o.defaultRoi,
         o.explicitRoi,
         o.defaultWavelengthDithers,
         o.explicitWavelengthDithers,
         o.defaultSpatialOffsets,
         o.explicitSpatialOffsets
        )
      )

  given Cogen[ScienceMode] =
    Cogen[Either[ScienceMode.GmosNorthLongSlit, ScienceMode.GmosSouthLongSlit]]
      .contramap {
        case n: ScienceMode.GmosNorthLongSlit => n.asLeft
        case s: ScienceMode.GmosSouthLongSlit => s.asRight
      }

}

object ArbScienceMode extends ArbScienceMode
