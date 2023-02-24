// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import algebra.instances.all.given
import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.implicits._
import coulomb._
import coulomb.conversion.ValueConversion
import coulomb.conversion.spire.*
import coulomb.ops.algebra.cats.quantity.given
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.overlay.refined.algebraic.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import eu.timepit.refined._
import eu.timepit.refined.cats._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.numeric._
import eu.timepit.refined.types.string._
import explore.model.syntax.all._
import fs2.data.csv._
import lucuma.core.enums._
import lucuma.core.math.Angle
import lucuma.core.math.BoundedInterval
import lucuma.core.math.BoundedInterval.*
import lucuma.core.math.Declination
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.units._
import lucuma.core.util.Enumerated
import lucuma.schemas.model.CentralWavelength
import monocle.Getter
import monocle.Lens
import monocle.macros.GenLens
import spire.math.Rational

sealed trait InstrumentRow derives Eq {
  def instrument: Instrument

  type Grating
  val grating: Grating

  type FPU
  val fpu: FPU

  type Filter
  val filter: Filter

  val site: Site

  def hasFilter: Boolean

  override def toString(): String = s"Mode: ${instrument.shortName}, $grating, $filter, $fpu"
}

case class GmosNorthSpectroscopyRow(
  grating: GmosNorthGrating,
  fpu:     GmosNorthFpu,
  filter:  Option[GmosNorthFilter]
) extends InstrumentRow {
  type Grating = GmosNorthGrating
  type Filter  = Option[GmosNorthFilter]
  type FPU     = GmosNorthFpu
  val instrument = Instrument.GmosNorth
  val site       = Site.GN
  val hasFilter  = filter.isDefined
}

case class GmosSouthSpectroscopyRow(
  grating: GmosSouthGrating,
  fpu:     GmosSouthFpu,
  filter:  Option[GmosSouthFilter]
) extends InstrumentRow {
  type Grating = GmosSouthGrating
  type Filter  = Option[GmosSouthFilter]
  type FPU     = GmosSouthFpu
  val instrument = Instrument.GmosSouth
  val site       = Site.GS
  val hasFilter  = filter.isDefined
}

case class Flamingos2SpectroscopyRow(grating: F2Disperser, filter: F2Filter) extends InstrumentRow {
  type Grating = F2Disperser
  type Filter  = F2Filter
  type FPU     = Unit
  val fpu        = ()
  val instrument = Instrument.Flamingos2
  val site       = Site.GS
  val hasFilter  = true
}

case class GpiSpectroscopyRow(grating: GpiDisperser, filter: GpiFilter) extends InstrumentRow {
  type Grating = GpiDisperser
  type Filter  = GpiFilter
  type FPU     = Unit
  val fpu        = ()
  val instrument = Instrument.Gpi
  val site       = Site.GN
  val hasFilter  = true
}

case class GnirsSpectroscopyRow(grating: GnirsDisperser, filter: GnirsFilter)
    extends InstrumentRow {
  type Grating = GnirsDisperser
  type Filter  = GnirsFilter
  type FPU     = Unit
  val fpu        = ()
  val instrument = Instrument.Gnirs
  val site       = Site.GN
  val hasFilter  = true
}

// Used for Instruments not fully defined
case class GenericSpectroscopyRow(i: Instrument, grating: String, filter: NonEmptyString)
    extends InstrumentRow {
  type Grating = String
  type Filter  = NonEmptyString
  type FPU     = Unit
  val fpu        = ()
  val instrument = i
  val site       = Site.GN
  val hasFilter  = true
}

object InstrumentRow {

  def decodeEnums[A: Enumerated, B](
    id:       B,
    criteria: (B, A) => Boolean
  ): Either[DecoderError, A] =
    Enumerated[A].all.find(criteria(id, _)).toRight(new DecoderError(s"Unknown enum $id"))

  def decodeOptionalenums[A: Enumerated](
    filter:   String,
    criteria: (String, A) => Boolean
  ): Either[DecoderError, Option[A]] =
    if (filter.isEmpty || filter.toLowerCase === "none") none.asRight
    else decodeEnums[A, String](filter, criteria).map(_.some)

  def decodeGmosSouthFilter(filter: NonEmptyString): Either[DecoderError, Option[GmosSouthFilter]] =
    decodeOptionalenums[GmosSouthFilter](filter.value, (i, f) => !f.obsolete && i === f.shortName)

  def decodeGmosSouthFPU(fpu: NonEmptyString): Either[DecoderError, GmosSouthFpu] =
    decodeEnums[GmosSouthFpu, String](fpu.value, (i, f) => i === f.shortName)

  def decodeGmosSouthGrating(grating: String): Either[DecoderError, GmosSouthGrating] =
    decodeEnums[GmosSouthGrating, String](grating, (i, f) => !f.obsolete && i === f.shortName)

  def decodeGmosNorthFilter(filter: NonEmptyString): Either[DecoderError, Option[GmosNorthFilter]] =
    decodeOptionalenums[GmosNorthFilter](filter.value, (i, f) => !f.obsolete && i === f.shortName)

  def decodeGmosNorthGrating(grating: String): Either[DecoderError, GmosNorthGrating] =
    decodeEnums[GmosNorthGrating, String](grating, (i, f) => !f.obsolete && i === f.shortName)

  def decodeGmosNorthFPU(fpu: NonEmptyString): Either[DecoderError, GmosNorthFpu] =
    decodeEnums[GmosNorthFpu, String](fpu.value, (i, f) => i === f.shortName)

  def decodeF2Filter(filter: NonEmptyString): Either[DecoderError, F2Filter] =
    decodeEnums[F2Filter, String](filter.value, (i, f) => !f.obsolete && i === f.shortName)

  def decodeF2Disperser(grating: String): Either[DecoderError, F2Disperser] =
    decodeEnums[F2Disperser, String](grating, _ === _.shortName)

  def decodeGpiFilter(filter: NonEmptyString): Either[DecoderError, GpiFilter] =
    decodeEnums[GpiFilter, String](filter.value, (i, f) => !f.obsolete && i === f.shortName)

  def decodeGpiDisperser(grating: String): Either[DecoderError, GpiDisperser] =
    decodeEnums[GpiDisperser, String](grating, _ === _.shortName)

  def decodeGnirsFilter(filter: NonEmptyString): Either[DecoderError, GnirsFilter] =
    decodeEnums[GnirsFilter, String](filter.value, _ === _.shortName)

  def decodeGnirsDisperser(grating: String): Either[DecoderError, GnirsDisperser] =
    decodeEnums[GnirsDisperser, String](grating, _ === _.shortName)

  def decode(
    instrument0: Instrument,
    grating0:    String,
    filter0:     NonEmptyString,
    fpu0:        NonEmptyString
  ): Either[DecoderError, InstrumentRow] =
    instrument0 match {
      case Instrument.GmosNorth  =>
        (decodeGmosNorthGrating(grating0), decodeGmosNorthFPU(fpu0), decodeGmosNorthFilter(filter0))
          .mapN(
            GmosNorthSpectroscopyRow.apply
          )
      case Instrument.GmosSouth  =>
        (decodeGmosSouthGrating(grating0), decodeGmosSouthFPU(fpu0), decodeGmosSouthFilter(filter0))
          .mapN(
            GmosSouthSpectroscopyRow.apply
          )
      case Instrument.Flamingos2 =>
        (decodeF2Disperser(grating0), decodeF2Filter(filter0)).mapN(
          Flamingos2SpectroscopyRow.apply
        )
      case Instrument.Gpi        =>
        (decodeGpiDisperser(grating0), decodeGpiFilter(filter0)).mapN(GpiSpectroscopyRow.apply)
      case Instrument.Gnirs      =>
        (decodeGnirsDisperser(grating0), decodeGnirsFilter(filter0)).mapN(
          GnirsSpectroscopyRow.apply
        )
      case i                     => GenericSpectroscopyRow(i, grating0, filter0).asRight
    }

  val instrument: Getter[InstrumentRow, Instrument] =
    Getter[InstrumentRow, Instrument](_.instrument)

  def grating: Getter[InstrumentRow, InstrumentRow#Grating] =
    Getter[InstrumentRow, InstrumentRow#Grating](_.grating)

  def filter: Getter[InstrumentRow, InstrumentRow#Filter] =
    Getter[InstrumentRow, InstrumentRow#Filter](_.filter)

}

case class SpectroscopyModeRow(
  id:                Int, // Give them a local id to simplify reusability
  instrument:        InstrumentRow,
  config:            NonEmptyString,
  focalPlane:        FocalPlane,
  capability:        Option[SpectroscopyCapabilities],
  ao:                ModeAO,
  minWavelength:     ModeWavelength,
  maxWavelength:     ModeWavelength,
  optimalWavelength: ModeWavelength,
  wavelengthDelta:   ModeWavelengthDelta,
  resolution:        PosInt,
  slitLength:        ModeSlitSize,
  slitWidth:         ModeSlitSize
) {
  // inline def calculatedCoverage: Quantity[NonNegBigDecimal, Micrometer] = wavelengthDelta

  inline def hasFilter: Boolean = instrument.hasFilter

  // This `should` always return a `some`, but if the row is wonky for some reason...
  def intervalCenter(cw: Wavelength): Option[CentralWavelength] =
    SpectroscopyModeRow
      .wavelengthInterval(cw)(this)
      .map(interval =>
        interval.lower.pm.value.value + (interval.upper.pm.value.value - interval.lower.pm.value.value) / 2
      )
      .flatMap(pms => Wavelength.fromIntPicometers(pms))
      .map(CentralWavelength(_))
}

object SpectroscopyModeRow {

  given ValueConversion[NonNegBigDecimal, BigDecimal] = _.value

  val instrumentRow: Lens[SpectroscopyModeRow, InstrumentRow] =
    GenLens[SpectroscopyModeRow](_.instrument)

  val instrument: Getter[SpectroscopyModeRow, Instrument] =
    instrumentRow.andThen(InstrumentRow.instrument)

  val config: Lens[SpectroscopyModeRow, NonEmptyString] =
    GenLens[SpectroscopyModeRow](_.config)

  val instrumentAndConfig: Getter[SpectroscopyModeRow, (Instrument, NonEmptyString)] =
    instrument.zip(config.asGetter)

  val slitWidth: Lens[SpectroscopyModeRow, ModeSlitSize] =
    GenLens[SpectroscopyModeRow](_.slitWidth)

  val slitLength: Lens[SpectroscopyModeRow, ModeSlitSize] =
    GenLens[SpectroscopyModeRow](_.slitLength)

  def grating: Getter[SpectroscopyModeRow, InstrumentRow#Grating] =
    instrumentRow.andThen(InstrumentRow.grating)

  def fpu: Lens[SpectroscopyModeRow, FocalPlane] =
    GenLens[SpectroscopyModeRow](_.focalPlane)

  def filter: Getter[SpectroscopyModeRow, InstrumentRow#Filter] =
    instrumentRow.andThen(InstrumentRow.filter)

  import lucuma.core.math.units.*

  def wavelengthInterval(
    λ: Wavelength
  ): SpectroscopyModeRow => Option[BoundedInterval[Wavelength]] =
    r =>
      val λr      = r.wavelengthDelta.value
      // Coverage of allowed wavelength
      // Can be simplified once coulomb-refined is available
      val λmin    = r.minWavelength.value
      val λmax    = r.maxWavelength.value
      val range   = λr.centeredAt(λ)
      // if we are below min clip but shift the range
      // same if we are above max
      // At any event we clip at min/max
      val shifted =
        if (range.lower < λmin)
          λr.startingAt(λmin)
        else if (range.upper > λmax)
          λr.endingAt(λmax)
        else
          range
      shifted.intersect(BoundedInterval.unsafeClosed(λmin, λmax))

  def resolution: Getter[SpectroscopyModeRow, PosInt] =
    Getter(_.resolution)
}

trait SpectroscopyModesMatrixDecoders extends Decoders {
  given CellDecoder[NonEmptyString] =
    CellDecoder.stringDecoder
      .emap { x =>
        refineV[NonEmpty](x).leftMap(s => new DecoderError(s))
      }

  given CellDecoder[NonEmptyList[FocalPlane]] =
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

  given CellDecoder[Option[SpectroscopyCapabilities]] =
    CellDecoder.stringDecoder
      .map {
        case "Nod&Shuffle" => SpectroscopyCapabilities.NodAndShuffle.some
        case "coronagraph" => SpectroscopyCapabilities.Coronagraphy.some
        case _             => none
      }

  given CsvRowDecoder[NonEmptyList[SpectroscopyModeRow], String] = (row: CsvRow[String]) =>
    for {
      di  <- row.as[String]("disperser")
      fi  <- row.as[NonEmptyString]("filter")
      fu  <- row.as[NonEmptyString]("fpu")
      i   <- row.as[Instrument]("instrument").flatMap(InstrumentRow.decode(_, di, fi, fu))
      s   <- row.as[NonEmptyString]("Config")
      fs  <- row.as[NonEmptyList[FocalPlane]]("Focal Plane")
      c   <- row.as[Option[SpectroscopyCapabilities]]("capabilities")
      a   <- row.as[ModeAO]("AO")
      min <- row.as[ModeWavelength]("wave min")
      max <- row.as[ModeWavelength]("wave max")
      wo  <- row.as[ModeWavelength]("wave optimal")
      wr  <- row.as[ModeWavelengthDelta]("wave coverage")
      r   <- row.as[PosInt]("resolution")
      sl  <- row.as[ModeSlitSize]("slit length")
      sw  <- row.as[ModeSlitSize]("slit width")
    } yield fs.map(f => // Ids are assigned later, after list is flattened.
      SpectroscopyModeRow(0, i, s, f, c, a, min, max, wo, wr, r, sl, sw)
    )

}

case class SpectroscopyModesMatrix(matrix: List[SpectroscopyModeRow]) {
  val ScoreBump   = Rational(1, 2)
  val FilterLimit = Wavelength.fromIntNanometers(650)

  def filtered(
    focalPlane:  Option[FocalPlane] = None,
    capability:  Option[SpectroscopyCapabilities] = None,
    iq:          Option[ImageQuality] = None,
    wavelength:  Option[Wavelength] = None,
    resolution:  Option[PosInt] = None,
    range:       Option[WavelengthDelta] = None,
    slitWidth:   Option[Angle] = None,
    declination: Option[Declination] = None
  ): List[SpectroscopyModeRow] = {
    // Criteria to filter the modes
    val filter: SpectroscopyModeRow => Boolean = r =>
      focalPlane.forall(f => r.focalPlane === f) &&
        r.capability === capability &&
        iq.forall(i => r.ao =!= ModeAO.AO || (i <= ImageQuality.PointTwo)) &&
        wavelength.forall(w => w >= r.minWavelength.value && w <= r.maxWavelength.value) &&
        resolution.forall(_ <= r.resolution) &&
        range.forall(_ <= r.wavelengthDelta.value) &&
        slitWidth.forall(_.toMicroarcseconds <= r.slitLength.value.toMicroarcseconds) &&
        declination.forall(r.instrument.site.inPreferredDeclination)

    // Calculates a score for each mode for sorting purposes. It is down in Rational space, we may change it to double as we don't really need high precission for this
    val score: SpectroscopyModeRow => Rational = { r =>
      // Difference in wavelength
      val deltaWave: BigDecimal       =
        wavelength
          .map(w => r.optimalWavelength.value.diff(w).abs.toNanometers.value)
          .getOrElse(BigDecimal(0))
      // Difference in slit width
      val deltaSlitWidth: Rational    =
        iq.map(i =>
          (Rational(r.slitWidth.value.toMicroarcseconds, 1000000) - i.toArcSeconds.value).abs
        ).getOrElse(Rational.zero)
      // Difference in resolution
      val deltaRes: BigDecimal        =
        resolution.foldMap(re => (re.value - r.resolution.value).abs)
      // give a bumpp to non-AO modes (but don't discard them)
      val aoScore: Rational           =
        if (iq.forall(i => r.ao =!= ModeAO.AO || (i <= ImageQuality.PointTwo))) ScoreBump
        else Rational.zero
      // If wavelength > 0.65mu, then prefer settings with a filter to avoid 2nd order contamination
      val filterScore: Rational       =
        (wavelength, FilterLimit)
          .mapN { (w, l) =>
            if (w >= l && r.hasFilter) ScoreBump else Rational.zero
          }
          .getOrElse(Rational.zero)
      // Wavelength matche
      val wavelengthScore: BigDecimal = wavelength
        .map(w => w.toNanometers.value.value / (w.toNanometers.value.value + deltaWave))
        .getOrElse(BigDecimal(0))
      // Resolution match
      val resolutionScore: Rational   = resolution
        .map(r => Rational(r.value / (r.value + deltaRes)))
        .getOrElse(Rational.zero)
      // Slit width match to the seeing (the IFU always matches)
      val slitWidthScore              =
        if (r.focalPlane === FocalPlane.IFU)
          Rational.one
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
