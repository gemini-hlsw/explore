// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import lucuma.core.enum._
import lucuma.core.math.Offset
import lucuma.schemas.decoders._
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

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
    lazy val Empty: GmosNorthLongSlit =
      GmosNorthLongSlit(none, none, none, none, none, none, none, none, List.empty, List.empty)

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

    implicit val gmosNLongSlitDecoder: Decoder[GmosNorthLongSlit] =
      Decoder.instance(c =>
        for {
          overrideGrating           <- c.downField("overrideGrating").as[Option[GmosNorthGrating]]
          overrideFilter            <- c.downField("overrideFilter").as[Option[GmosNorthFilter]]
          overrideFpu               <- c.downField("overrideFpu").as[Option[GmosNorthFpu]]
          explicitXBin              <- c.downField("explicitXBin").as[Option[GmosXBinning]]
          explicitYBin              <- c.downField("explicitYBin").as[Option[GmosYBinning]]
          explicitAmpReadMode       <- c.downField("explicitAmpReadMode").as[Option[GmosAmpReadMode]]
          explicitAmpGain           <- c.downField("explicitAmpGain").as[Option[GmosAmpGain]]
          explicitRoi               <- c.downField("explicitRoi").as[Option[GmosRoi]]
          explicitWavelengthDithers <-
            c.downField("explicitWavelengthDithers")
              .as[Option[List[Int]]]
              .map(_.getOrElse(List.empty))
          explicitSpatialOffsets    <-
            c.downField("explicitSpatialOffsets")
              .as[Option[List[Offset.Q]]]
              .map(_.getOrElse(List.empty))
        } yield GmosNorthLongSlit(
          overrideGrating,
          overrideFilter,
          overrideFpu,
          explicitXBin,
          explicitYBin,
          explicitAmpReadMode,
          explicitAmpGain,
          explicitRoi,
          explicitWavelengthDithers,
          explicitSpatialOffsets
        )
      )

    val overrideGrating: Lens[GmosNorthLongSlit, Option[GmosNorthGrating]] =
      Focus[GmosNorthLongSlit](_.overrideGrating)

    val overrideFilter: Lens[GmosNorthLongSlit, Option[GmosNorthFilter]] =
      Focus[GmosNorthLongSlit](_.overrideFilter)

    val overrideFpu: Lens[GmosNorthLongSlit, Option[GmosNorthFpu]] =
      Focus[GmosNorthLongSlit](_.overrideFpu)

    val explicitXBin: Lens[GmosNorthLongSlit, Option[GmosXBinning]] =
      Focus[GmosNorthLongSlit](_.explicitXBin)

    val explicitYBin: Lens[GmosNorthLongSlit, Option[GmosYBinning]] =
      Focus[GmosNorthLongSlit](_.explicitYBin)

    val explicitAmpReadMode: Lens[GmosNorthLongSlit, Option[GmosAmpReadMode]] =
      Focus[GmosNorthLongSlit](_.explicitAmpReadMode)

    val explicitAmpGain: Lens[GmosNorthLongSlit, Option[GmosAmpGain]] =
      Focus[GmosNorthLongSlit](_.explicitAmpGain)

    val explicitRoi: Lens[GmosNorthLongSlit, Option[GmosRoi]] =
      Focus[GmosNorthLongSlit](_.explicitRoi)

    val explicitWavelengthDithers: Lens[GmosNorthLongSlit, List[Int]] =
      Focus[GmosNorthLongSlit](_.explicitWavelengthDithers)

    val explicitSpatialOffsets: Lens[GmosNorthLongSlit, List[Offset.Q]] =
      Focus[GmosNorthLongSlit](_.explicitSpatialOffsets)
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
    lazy val Empty: GmosSouthLongSlit =
      GmosSouthLongSlit(none, none, none, none, none, none, none, none, List.empty, List.empty)

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

    implicit val gmosSLongSlitDecoder: Decoder[GmosSouthLongSlit] =
      Decoder.instance(c =>
        for {
          overrideGrating           <- c.downField("overrideGrating").as[Option[GmosSouthGrating]]
          overrideFilter            <- c.downField("overrideFilter").as[Option[GmosSouthFilter]]
          overrideFpu               <- c.downField("overrideFpu").as[Option[GmosSouthFpu]]
          explicitXBin              <- c.downField("explicitXBin").as[Option[GmosXBinning]]
          explicitYBin              <- c.downField("explicitYBin").as[Option[GmosYBinning]]
          explicitAmpReadMode       <- c.downField("explicitAmpReadMode").as[Option[GmosAmpReadMode]]
          explicitAmpGain           <- c.downField("explicitAmpGain").as[Option[GmosAmpGain]]
          explicitRoi               <- c.downField("explicitRoi").as[Option[GmosRoi]]
          explicitWavelengthDithers <-
            c.downField("explicitWavelengthDithers")
              .as[Option[List[Int]]]
              .map(_.getOrElse(List.empty))
          explicitSpatialOffsets    <-
            c.downField("explicitSpatialOffsets")
              .as[Option[List[Offset.Q]]]
              .map(_.getOrElse(List.empty))
        } yield GmosSouthLongSlit(
          overrideGrating,
          overrideFilter,
          overrideFpu,
          explicitXBin,
          explicitYBin,
          explicitAmpReadMode,
          explicitAmpGain,
          explicitRoi,
          explicitWavelengthDithers,
          explicitSpatialOffsets
        )
      )

    val overrideGrating: Lens[GmosSouthLongSlit, Option[GmosSouthGrating]] =
      Focus[GmosSouthLongSlit](_.overrideGrating)

    val overrideFilter: Lens[GmosSouthLongSlit, Option[GmosSouthFilter]] =
      Focus[GmosSouthLongSlit](_.overrideFilter)

    val overrideFpu: Lens[GmosSouthLongSlit, Option[GmosSouthFpu]] =
      Focus[GmosSouthLongSlit](_.overrideFpu)

    val explicitXBin: Lens[GmosSouthLongSlit, Option[GmosXBinning]] =
      Focus[GmosSouthLongSlit](_.explicitXBin)

    val explicitYBin: Lens[GmosSouthLongSlit, Option[GmosYBinning]] =
      Focus[GmosSouthLongSlit](_.explicitYBin)

    val explicitAmpReadMode: Lens[GmosSouthLongSlit, Option[GmosAmpReadMode]] =
      Focus[GmosSouthLongSlit](_.explicitAmpReadMode)

    val explicitAmpGain: Lens[GmosSouthLongSlit, Option[GmosAmpGain]] =
      Focus[GmosSouthLongSlit](_.explicitAmpGain)

    val explicitRoi: Lens[GmosSouthLongSlit, Option[GmosRoi]] =
      Focus[GmosSouthLongSlit](_.explicitRoi)

    val explicitWavelengthDithers: Lens[GmosSouthLongSlit, List[Int]] =
      Focus[GmosSouthLongSlit](_.explicitWavelengthDithers)

    val explicitSpatialOffsets: Lens[GmosSouthLongSlit, List[Offset.Q]] =
      Focus[GmosSouthLongSlit](_.explicitSpatialOffsets)
  }

  val gmosNorthLongSlit: Prism[ScienceModeAdvanced, GmosNorthLongSlit] =
    GenPrism[ScienceModeAdvanced, GmosNorthLongSlit]

  val gmosSouthLongSlit: Prism[ScienceModeAdvanced, GmosSouthLongSlit] =
    GenPrism[ScienceModeAdvanced, GmosSouthLongSlit]

}
