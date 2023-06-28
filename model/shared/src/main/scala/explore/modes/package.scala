// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.Eq
import cats.Order
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.*
import fs2.data.csv.*
import lucuma.core.enums.Instrument
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.optics.Wedge
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType

package modes {
  object ModeWavelength extends NewType[Wavelength]:
    extension (w: ModeWavelength)
      def toString: String = s"${w.value.toMicrometers.value.value.toDouble} μm"
  type ModeWavelength = ModeWavelength.Type

  object ModeWavelengthDelta extends NewType[WavelengthDelta]
  type ModeWavelengthDelta = ModeWavelengthDelta.Type

  object ModeSlitSize extends NewType[Angle]:
    val milliarcseconds: Wedge[Angle, BigDecimal] =
      Angle.milliarcseconds
        .imapB(_.underlying.movePointRight(3).intValue,
               n => new java.math.BigDecimal(n).movePointLeft(3)
        )

    given Order[ModeSlitSize] = Order.by(_.value.toMicroarcseconds)

    extension (size: ModeSlitSize)
      def toString: String = s"${Angle.milliarcseconds.get(size.value) / 1000.0} arcsec"

  type ModeSlitSize = ModeSlitSize.Type

  enum ModeAO(val tag: String):
    case NoAO extends ModeAO("no_ao")
    case AO   extends ModeAO("ao")

  object ModeAO:
    given Enumerated[ModeAO] =
      Enumerated.from(NoAO, AO).withTag(_.tag)

  trait Decoders {
    given CellDecoder[PosInt] =
      CellDecoder.intDecoder
        .emap { x =>
          refineV[Positive](x).leftMap(s => new DecoderError(s))
        }

    given CellDecoder[NonNegInt] =
      CellDecoder.intDecoder
        .emap { x =>
          refineV[NonNegative](x).leftMap(s => new DecoderError(s))
        }

    given CellDecoder[Instrument] =
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

    given CellDecoder[ModeAO] =
      CellDecoder.stringDecoder
        .map {
          case "yes" => ModeAO.AO
          case _     => ModeAO.NoAO
        }

    val micrometerToPicometerDecoder: CellDecoder[PosInt] =
      CellDecoder.bigDecimalDecoder.emap(x =>
        PosInt
          .from((x * 1000000).intValue)
          .leftMap(msg => new DecoderError(s"Invalid wavelength value $x: $msg"))
      )

    val micrometerWavelengthDecoder: CellDecoder[Wavelength] =
      micrometerToPicometerDecoder.map(Wavelength(_))

    given CellDecoder[ModeWavelength] = micrometerWavelengthDecoder.map(ModeWavelength(_))

    given CellDecoder[ModeWavelengthDelta] =
      micrometerToPicometerDecoder.map(pm => ModeWavelengthDelta(WavelengthDelta(pm)))

    given CellDecoder[PosBigDecimal] =
      CellDecoder.bigDecimalDecoder
        .emap { x =>
          refineV[Positive](x).leftMap(s => new DecoderError(s))
        }

    given CellDecoder[NonNegBigDecimal] =
      CellDecoder.bigDecimalDecoder
        .emap { x =>
          refineV[NonNegative](x).leftMap(s => new DecoderError(s))
        }

    val arcsecDecoder: CellDecoder[Angle] =
      CellDecoder.bigDecimalDecoder.map(x => Angle.milliarcseconds.reverseGet((x * 1000).intValue))

    given CellDecoder[ModeSlitSize] =
      arcsecDecoder.map(ModeSlitSize(_))

  }
}
