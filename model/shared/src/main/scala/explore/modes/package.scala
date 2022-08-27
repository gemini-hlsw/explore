// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.*
import fs2.data.csv.*
import lucuma.core.enums.Instrument
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.optics.Wedge
import lucuma.core.util.Enumerated
import monocle.Lens
import monocle.macros.GenLens

package modes {

  case class ModeWavelength(w: Wavelength) {
    override def toString: String = s"${w.micrometer.value.toDouble} μm"
  }

  case class ModeSlitSize(size: Angle) {
    override def toString: String = s"${Angle.milliarcseconds.get(size) / 1000.0} arcsec"
  }

  object ModeSlitSize {
    val size: Lens[ModeSlitSize, Angle] = GenLens[ModeSlitSize](_.size)

    val milliarcseconds: Wedge[Angle, BigDecimal] =
      Angle.milliarcseconds
        .imapB(_.underlying.movePointRight(3).intValue,
               n => new java.math.BigDecimal(n).movePointLeft(3)
        )
  }

  enum ModeAO(val tag: String):
    case NoAO extends ModeAO("noao")
    case AO   extends ModeAO("ao")

  object ModeAO:
    given Enumerated[ModeAO] =
      Enumerated.from(NoAO, AO).withTag(_.tag)

  trait Decoders {
    implicit val posIntDecoder: CellDecoder[PosInt] =
      CellDecoder.intDecoder
        .emap { x =>
          refineV[Positive](x).leftMap(s => new DecoderError(s))
        }

    implicit val nonNegativeIntDecoder: CellDecoder[NonNegInt] =
      CellDecoder.intDecoder
        .emap { x =>
          refineV[NonNegative](x).leftMap(s => new DecoderError(s))
        }

    implicit val instDecoder: CellDecoder[Instrument] =
      CellDecoder.stringDecoder
        .emap {
          case "FLAMINGOS2" => Instrument.Flamingos2.asRight
          case "GSAOI"      => Instrument.Gsaoi.asRight
          case "GMOS-S"     => Instrument.GmosSouth.asRight
          case "GMOS-N"     => Instrument.GmosNorth.asRight
          case "GPI"        => Instrument.Gpi.asRight
          case "NIFS"       => Instrument.Nifs.asRight
          case "SCORPIO"    => Instrument.Scorpio.asRight
          case "GNIRS"      => Instrument.Gnirs.asRight
          case s"GNIRS $_"  => Instrument.Gnirs.asRight
          case x            => new DecoderError(s"Unknown instrument $x").asLeft
        }

    implicit val modeAODecoder: CellDecoder[ModeAO] =
      CellDecoder.stringDecoder
        .map {
          case "yes" => ModeAO.AO
          case _     => ModeAO.NoAO
        }

    val micrometerDecoder: CellDecoder[Wavelength] =
      CellDecoder.bigDecimalDecoder.emap(x =>
        Wavelength.fromPicometers
          .getOption((x * 1000000).intValue)
          .toRight(new DecoderError(s"Invalid wavelength value $x"))
      )

    implicit val wavelength: CellDecoder[ModeWavelength] =
      micrometerDecoder.map(ModeWavelength.apply)

    implicit val posBigDecimalDecoder: CellDecoder[PosBigDecimal] =
      CellDecoder.bigDecimalDecoder
        .emap { x =>
          refineV[Positive](x).leftMap(s => new DecoderError(s))
        }

    implicit val nonNegativeBigDecimalDecoder: CellDecoder[NonNegBigDecimal] =
      CellDecoder.bigDecimalDecoder
        .emap { x =>
          refineV[NonNegative](x).leftMap(s => new DecoderError(s))
        }

    val arcsecDecoder: CellDecoder[Angle] =
      CellDecoder.bigDecimalDecoder.map(x => Angle.milliarcseconds.reverseGet((x * 1000).intValue))

    implicit val swDecoder: CellDecoder[ModeSlitSize] =
      arcsecDecoder.map(ModeSlitSize.apply)

  }
}
