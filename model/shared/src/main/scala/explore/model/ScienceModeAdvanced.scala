// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto._
import lucuma.core.enum._
import lucuma.core.math.Offset
import lucuma.schemas.decoders._

sealed trait ScienceModeAdvanced extends Product with Serializable

object ScienceModeAdvanced {
  implicit val scienceModeAdvancedEq: Eq[ScienceModeAdvanced] =
    Eq.instance {
      case (a: GmosNorthLongSlit, b: GmosNorthLongSlit) => a === b
      case (a: GmosSouthLongSlit, b: GmosSouthLongSlit) => a === b
      case _                                            => false
    }

  final case class GmosNorthLongSlit(
    overrideGrating:           Option[GmosNorthGrating],
    overrideFilter:            Option[GmosNorthFilter],
    overrideFpu:               Option[GmosNorthFpu],
    explicitXBin:              Option[GmosXBinning],
    explicitYBin:              Option[GmosYBinning],
    explicitAmpReadMode:       Option[GmosAmpReadMode],
    explicitAmpGain:           Option[GmosAmpGain],
    explicitRoi:               Option[GmosRoi],
    explicitWavelengthDithers: List[Int],
    explicitSpatialOffsets:    List[Offset.Q]
  ) extends ScienceModeAdvanced

  object GmosNorthLongSlit {
    implicit val gmosNLongSlitEq: Eq[GmosNorthLongSlit] =
      Eq.by(x =>
        (x.overrideGrating,
         x.overrideFilter,
         x.overrideFpu,
         x.explicitXBin,
         x.explicitYBin,
         x.explicitAmpReadMode,
         x.explicitAmpGain,
         x.explicitRoi,
         x.explicitWavelengthDithers,
         x.explicitSpatialOffsets
        )
      )

    implicit val gmosNLongSlitDecoder: Decoder[GmosNorthLongSlit] = deriveDecoder
  }

  final case class GmosSouthLongSlit(
    overrideGrating:           Option[GmosSouthGrating],
    overrideFilter:            Option[GmosSouthFilter],
    overrideFpu:               Option[GmosSouthFpu],
    explicitXBin:              Option[GmosXBinning],
    explicitYBin:              Option[GmosYBinning],
    explicitAmpReadMode:       Option[GmosAmpReadMode],
    explicitAmpGain:           Option[GmosAmpGain],
    explicitRoi:               Option[GmosRoi],
    explicitWavelengthDithers: List[Int],
    explicitSpatialOffsets:    List[Offset.Q]
  ) extends ScienceModeAdvanced

  object GmosSouthLongSlit {
    implicit val gmosSLongSlitEq: Eq[GmosSouthLongSlit] =
      Eq.by(x =>
        (x.overrideGrating,
         x.overrideFilter,
         x.overrideFpu,
         x.explicitXBin,
         x.explicitYBin,
         x.explicitAmpReadMode,
         x.explicitAmpGain,
         x.explicitRoi,
         x.explicitWavelengthDithers,
         x.explicitSpatialOffsets
        )
      )

    implicit val gmosSLongSlitDecoder: Decoder[GmosSouthLongSlit] = deriveDecoder
  }
}
