// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.data.NonEmptyList
import cats.implicits._
import coulomb._
import eu.timepit.refined._
import eu.timepit.refined.cats._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.numeric._
import eu.timepit.refined.types.string._
import explore.model.enum.SpectroscopyCapabilities
import fs2.data.csv._
import lucuma.core.enum.F2Disperser
import lucuma.core.enum.F2Filter
import lucuma.core.enum.GmosNorthDisperser
import lucuma.core.enum.GmosNorthFilter
import lucuma.core.enum.GmosSouthDisperser
import lucuma.core.enum.GmosSouthFilter
import lucuma.core.enum.GnirsDisperser
import lucuma.core.enum.GnirsFilter
import lucuma.core.enum.GpiDisperser
import lucuma.core.enum.GpiFilter
import lucuma.core.enum.ImageQuality
import lucuma.core.enum.Instrument
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units._
import lucuma.core.util.Enumerated
import monocle.Getter
import monocle.Lens
import monocle.macros.GenLens
import spire.math.Rational

sealed trait FocalPlane extends Product with Serializable

object FocalPlane {
  case object SingleSlit   extends FocalPlane
  case object MultipleSlit extends FocalPlane
  case object IFU          extends FocalPlane

  /** @group Typeclass Instances */
  implicit val FocalPlaneEnumerated: Enumerated[FocalPlane] =
    Enumerated.of(SingleSlit, MultipleSlit, IFU)
}

trait InstrumentRow {
  def instrument: Instrument

  type Disperser
  val disperser: Disperser

  type Filter
  val filter: Filter
}

object InstrumentRow {

  def decodeEnum[A: Enumerated, B](
    id:       B,
    criteria: (B, A) => Boolean
  ): Either[DecoderError, A] =
    Enumerated[A].all.find(criteria(id, _)).toRight(new DecoderError(s"Unknown enum $id"))

  def decodeOptionalEnum[A: Enumerated](
    filter:   String,
    criteria: (String, A) => Boolean
  ): Either[DecoderError, Option[A]] =
    if (filter.isEmpty || filter.toLowerCase === "none") none.asRight
    else decodeEnum[A, String](filter, criteria).map(_.some)

  def decodeGmosSouthFilter(filter: NonEmptyString): Either[DecoderError, Option[GmosSouthFilter]] =
    decodeOptionalEnum[GmosSouthFilter](filter.value, (i, f) => !f.obsolete && i === f.shortName)

  def decodeGmosSouthDisperser(disperser: String): Either[DecoderError, GmosSouthDisperser] =
    decodeEnum[GmosSouthDisperser, String](disperser, (i, f) => !f.obsolete && i === f.shortName)

  def decodeGmosNorthFilter(filter: NonEmptyString): Either[DecoderError, Option[GmosNorthFilter]] =
    decodeOptionalEnum[GmosNorthFilter](filter.value, (i, f) => !f.obsolete && i === f.shortName)

  def decodeGmosNorthDisperser(disperser: String): Either[DecoderError, GmosNorthDisperser] =
    decodeEnum[GmosNorthDisperser, String](disperser, (i, f) => !f.obsolete && i === f.shortName)

  def decodeF2Filter(filter: NonEmptyString): Either[DecoderError, F2Filter] =
    decodeEnum[F2Filter, String](filter.value, (i, f) => !f.obsolete && i === f.shortName)

  def decodeF2Disperser(disperser: String): Either[DecoderError, F2Disperser] =
    decodeEnum[F2Disperser, String](disperser, _ === _.shortName)

  def decodeGpiFilter(filter: NonEmptyString): Either[DecoderError, GpiFilter] =
    decodeEnum[GpiFilter, String](filter.value, (i, f) => !f.obsolete && i === f.shortName)

  def decodeGpiDisperser(disperser: String): Either[DecoderError, GpiDisperser] =
    decodeEnum[GpiDisperser, String](disperser, _ === _.shortName)

  def decodeGnirsFilter(filter: NonEmptyString): Either[DecoderError, GnirsFilter] =
    decodeEnum[GnirsFilter, String](filter.value, _ === _.shortName)

  def decodeGnirsDisperser(disperser: String): Either[DecoderError, GnirsDisperser] =
    decodeEnum[GnirsDisperser, String](disperser, _ === _.shortName)

  def decode(
    instrument0: Instrument,
    disperser0:  String,
    filter0:     NonEmptyString
  ): Either[DecoderError, InstrumentRow] =
    instrument0 match {
      case i @ Instrument.GmosNorth  =>
        (decodeGmosNorthDisperser(disperser0), decodeGmosNorthFilter(filter0)).mapN { case (d, f) =>
          new InstrumentRow {
            val instrument = i
            type Disperser = GmosNorthDisperser
            val disperser = d
            type Filter = Option[GmosNorthFilter]
            val filter = f
          }
        }
      case i @ Instrument.GmosSouth  =>
        (decodeGmosSouthDisperser(disperser0), decodeGmosSouthFilter(filter0)).mapN { case (d, f) =>
          new InstrumentRow {
            val instrument = i
            type Disperser = GmosSouthDisperser
            val disperser = d
            type Filter = Option[GmosSouthFilter]
            val filter = f
          }
        }
      case i @ Instrument.Flamingos2 =>
        (decodeF2Disperser(disperser0), decodeF2Filter(filter0)).mapN { case (d, f) =>
          new InstrumentRow {
            val instrument = i
            type Disperser = F2Disperser
            val disperser = d
            type Filter = F2Filter
            val filter = f
          }
        }
      case i @ Instrument.Gpi        =>
        (decodeGpiDisperser(disperser0), decodeGpiFilter(filter0)).mapN { case (d, f) =>
          new InstrumentRow {
            val instrument = i
            type Disperser = GpiDisperser
            val disperser = d
            type Filter = GpiFilter
            val filter = f
          }
        }
      case i @ Instrument.Gnirs      =>
        (decodeGnirsDisperser(disperser0), decodeGnirsFilter(filter0)).mapN { case (d, f) =>
          new InstrumentRow {
            val instrument = i
            type Disperser = GnirsDisperser
            val disperser = d
            type Filter = GnirsFilter
            val filter = f
          }
        }
      case i                         =>
        new InstrumentRow {
          val instrument = i
          type Disperser = String
          val disperser = disperser0
          type Filter = String
          val filter = filter0.value
        }.asRight
    }

  val instrument: Getter[InstrumentRow, Instrument] =
    Getter[InstrumentRow, Instrument](_.instrument)

  def disperser: Getter[InstrumentRow, InstrumentRow#Disperser] =
    Getter[InstrumentRow, InstrumentRow#Disperser](_.disperser)

  def filter: Getter[InstrumentRow, InstrumentRow#Filter] =
    Getter[InstrumentRow, InstrumentRow#Filter](_.filter)

}

case class SpectroscopyModeRow(
  instrument:        InstrumentRow,
  config:            NonEmptyString,
  focalPlane:        NonEmptyList[FocalPlane],
  capabilities:      Option[SpectroscopyCapabilities],
  ao:                ModeAO,
  minWavelength:     ModeWavelength,
  maxWavelength:     ModeWavelength,
  optimalWavelength: ModeWavelength,
  wavelengthRange:   Quantity[NonNegBigDecimal, Micrometer],
  resolution:        PosBigDecimal,
  slitLength:        ModeSlitSize,
  slitWidth:         ModeSlitSize
) {
  def calculatedRange: Quantity[NonNegBigDecimal, Micrometer] = wavelengthRange

  val hasFilter: Boolean = instrument.filter match {
    case _: None.type => false
    case _            => true
  }

}

object SpectroscopyModeRow {
  val instrumentRow: Lens[SpectroscopyModeRow, InstrumentRow] =
    GenLens[SpectroscopyModeRow](_.instrument)

  val instrument: Getter[SpectroscopyModeRow, Instrument] =
    instrumentRow.composeGetter(InstrumentRow.instrument)

  val config: Lens[SpectroscopyModeRow, NonEmptyString] =
    GenLens[SpectroscopyModeRow](_.config)

  val instrumentAndConfig: Getter[SpectroscopyModeRow, (Instrument, NonEmptyString)] =
    instrument.zip(config.asGetter)

  val slitWidth: Lens[SpectroscopyModeRow, ModeSlitSize] =
    GenLens[SpectroscopyModeRow](_.slitWidth)

  val slitLength: Lens[SpectroscopyModeRow, ModeSlitSize] =
    GenLens[SpectroscopyModeRow](_.slitLength)

  def disperser: Getter[SpectroscopyModeRow, InstrumentRow#Disperser] =
    instrumentRow.composeGetter(InstrumentRow.disperser)

  def filter: Getter[SpectroscopyModeRow, InstrumentRow#Filter] =
    instrumentRow.composeGetter(InstrumentRow.filter)

  def range: Getter[SpectroscopyModeRow, Quantity[NonNegBigDecimal, Micrometer]] =
    Getter(_.calculatedRange)

  def resolution: Getter[SpectroscopyModeRow, PosBigDecimal] =
    Getter(_.resolution)
}

trait SpectroscopyModesMatrixDecoders extends Decoders {
  implicit val nonEmptyStringDecoder: CellDecoder[NonEmptyString] =
    CellDecoder.stringDecoder
      .emap { x =>
        refineV[NonEmpty](x).leftMap(s => new DecoderError(s))
      }

  implicit val focalPlaneDecoder: CellDecoder[NonEmptyList[FocalPlane]] =
    CellDecoder.stringDecoder
      .emap { r =>
        r.toLowerCase
          .replace("\\s", "")
          .trim
          .split(",")
          .toList
          .collect {
            case "singleslit" => FocalPlane.SingleSlit
            case "multislit"  => FocalPlane.MultipleSlit
            case "ifu"        => FocalPlane.IFU
          } match {
          case h :: t => NonEmptyList.of(h, t: _*).asRight
          case Nil    => new DecoderError(s"Unknown focal plane $r").asLeft
        }
      }

  implicit val capabilitiesDecoder: CellDecoder[Option[SpectroscopyCapabilities]] =
    CellDecoder.stringDecoder
      .map {
        case "Nod&Shuffle" => SpectroscopyCapabilities.NodAndShuffle.some
        case "coronagraph" => SpectroscopyCapabilities.Corongraphy.some
        case _             => none
      }

  implicit object SpectroscopySpectroscopyModeRowDecoder
      extends CsvRowDecoder[SpectroscopyModeRow, String] {
    def apply(row: CsvRow[String]): DecoderResult[SpectroscopyModeRow] =
      for {
        di  <- row.as[String]("disperser")
        fi  <- row.as[NonEmptyString]("filter")
        i   <- row.as[Instrument]("instrument").flatMap(InstrumentRow.decode(_, di, fi))
        s   <- row.as[NonEmptyString]("Config")
        f   <- row.as[NonEmptyList[FocalPlane]]("Focal Plane")
        c   <- row.as[Option[SpectroscopyCapabilities]]("capabilities")
        a   <- row.as[ModeAO]("AO")
        min <- row.as[ModeWavelength]("wave min")
        max <- row.as[ModeWavelength]("wave max")
        wo  <- row.as[ModeWavelength]("wave optimal")
        wr  <- row.as[NonNegBigDecimal]("wave range").map(_.withUnit[Micrometer])
        r   <- row.as[PosBigDecimal]("resolution")
        sl  <- row.as[ModeSlitSize]("slit length")
        sw  <- row.as[ModeSlitSize]("slit width")
      } yield SpectroscopyModeRow(i, s, f, c, a, min, max, wo, wr, r, sl, sw)
  }
}

final case class SpectroscopyModesMatrix(matrix: List[SpectroscopyModeRow]) {
  val ScoreBump   = Rational(1, 2)
  val FilterLimit = Wavelength.fromNanometers(650)

  def filtered(
    focalPlane:   Option[FocalPlane],
    capabilities: Option[SpectroscopyCapabilities],
    iq:           Option[ImageQuality],
    wavelength:   Option[Wavelength],
    resolution:   Option[PosBigDecimal],
    range:        Option[Quantity[NonNegBigDecimal, Micrometer]],
    slitWidth:    Option[Angle]
  ): List[SpectroscopyModeRow] = {
    // Criteria to filter the modes
    val filter: SpectroscopyModeRow => Boolean = r => {
      focalPlane.forall(f => r.focalPlane.exists(_ === f)) &&
        r.capabilities === capabilities &&
        iq.forall(i => r.ao =!= ModeAO.AO || (i <= ImageQuality.PointTwo)) &&
        wavelength.forall(w => w >= r.minWavelength.w && w <= r.maxWavelength.w) &&
        resolution.forall(_ <= r.resolution) &&
        range.forall(_ <= r.wavelengthRange) &&
        slitWidth.forall(_.toMicroarcseconds <= r.slitLength.size.toMicroarcseconds)
    }

    // Calculates a score for each mode for sorting purposes. It is down in Rational space, we may change it to double as we don't really need high precission for this
    val score: SpectroscopyModeRow => Rational = { r =>
      // Difference in wavelength
      val deltaWave: Rational       =
        wavelength
          .map(w => (r.optimalWavelength.w.nanometer - w.nanometer).value.abs)
          .getOrElse(Rational.zero)
      // Difference in slit width
      val deltaSlitWidth: Rational  =
        iq.map(i =>
          (Rational(r.slitWidth.size.toMicroarcseconds, 1000000) - i.toArcSeconds.value).abs
        ).getOrElse(Rational.zero)
      // Difference in resolution
      val deltaRes: BigDecimal      =
        resolution.foldMap(re => (re.value - r.resolution.value).abs)
      // give a bumpp to non-AO modes (but don't discard them)
      val aoScore: Rational         =
        if (iq.forall(i => r.ao =!= ModeAO.AO || (i <= ImageQuality.PointTwo))) ScoreBump
        else Rational.zero
      // If wavelength > 0.65mu, then prefer settings with a filter to avoid 2nd order contamination
      val filterScore: Rational     =
        (wavelength, FilterLimit)
          .mapN { (w, l) =>
            if (w >= l && r.hasFilter) ScoreBump else Rational.zero
          }
          .getOrElse(Rational.zero)
      // Wavelength matche
      val wavelengthScore: Rational = wavelength
        .map(w => (w.nanometer.value / (w.nanometer.value + deltaWave)))
        .getOrElse(Rational.zero)
      // Resolution match
      val resolutionScore: Rational = resolution
        .map(r => Rational(r.value / (r.value + deltaRes)))
        .getOrElse(Rational.zero)
      // Slit width match to the seeing (the IFU always matches)
      val slitWidthScore            =
        if (r.focalPlane.forall(_ === FocalPlane.IFU)) Rational.one
        else
          iq.map(i => Rational(i.toArcSeconds.value / (i.toArcSeconds.value + deltaSlitWidth)))
            .getOrElse(Rational.zero)
      aoScore + wavelengthScore + filterScore + resolutionScore + slitWidthScore
    }

    matrix
      .filter(filter)  // Only include matches
      .fproduct(score) // Give it a score
      .sortBy(_._2)    // Sort by score
      .map(_._1)
      .reverse
  }
}

object SpectroscopyModesMatrix extends SpectroscopyModesMatrixPlatform {
  val empty: SpectroscopyModesMatrix = SpectroscopyModesMatrix(Nil)
}
