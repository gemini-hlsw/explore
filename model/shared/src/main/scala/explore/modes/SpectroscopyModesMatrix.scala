// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Eq
import cats.Order
import cats.derived.*
import cats.implicits.*
import coulomb.*
import coulomb.conversion.ValueConversion
import eu.timepit.refined.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.numeric.*
import eu.timepit.refined.types.string.*
import explore.model.syntax.all.*
import fs2.data.csv.*
import io.circe.Decoder
import io.circe.refined.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.BoundedInterval
import lucuma.core.math.BoundedInterval.*
import lucuma.core.math.Declination
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.units.*
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.wavelength.decoder.given
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

  type Override
  def modeOverrides: Option[Override] = None

  override def toString(): String = s"Mode: ${instrument.shortName}, $grating, $filter, $fpu"
}

sealed trait InstrumentOverrides derives Eq
case class GmosSpectroscopyOverrides(ccdMode: Option[GmosCcdMode], roi: Option[GmosRoi])
    extends InstrumentOverrides derives Eq

case class GmosNorthSpectroscopyRow(
  grating:                    GmosNorthGrating,
  fpu:                        GmosNorthFpu,
  filter:                     Option[GmosNorthFilter],
  override val modeOverrides: Option[GmosSpectroscopyOverrides]
) extends InstrumentRow {
  type Grating  = GmosNorthGrating
  type Filter   = Option[GmosNorthFilter]
  type FPU      = GmosNorthFpu
  type Override = GmosSpectroscopyOverrides
  val instrument = Instrument.GmosNorth
  val site       = Site.GN
  val hasFilter  = filter.isDefined

}

case class GmosSouthSpectroscopyRow(
  grating:                    GmosSouthGrating,
  fpu:                        GmosSouthFpu,
  filter:                     Option[GmosSouthFilter],
  override val modeOverrides: Option[GmosSpectroscopyOverrides]
) extends InstrumentRow {
  type Grating  = GmosSouthGrating
  type Filter   = Option[GmosSouthFilter]
  type FPU      = GmosSouthFpu
  type Override = GmosSpectroscopyOverrides
  val instrument = Instrument.GmosSouth
  val site       = Site.GS
  val hasFilter  = filter.isDefined
}

case class Flamingos2SpectroscopyRow(grating: F2Disperser, filter: F2Filter) extends InstrumentRow {
  type Grating  = F2Disperser
  type Filter   = F2Filter
  type FPU      = Unit
  type Override = Unit
  val fpu        = ()
  val instrument = Instrument.Flamingos2
  val site       = Site.GS
  val hasFilter  = true
}

case class GpiSpectroscopyRow(grating: GpiDisperser, filter: GpiFilter) extends InstrumentRow {
  type Grating  = GpiDisperser
  type Filter   = GpiFilter
  type FPU      = Unit
  type Override = Unit
  val fpu        = ()
  val instrument = Instrument.Gpi
  val site       = Site.GN
  val hasFilter  = true
}

case class GnirsSpectroscopyRow(grating: GnirsDisperser, filter: GnirsFilter)
    extends InstrumentRow {
  type Grating  = GnirsDisperser
  type Filter   = GnirsFilter
  type FPU      = Unit
  type Override = Unit
  val fpu        = ()
  val instrument = Instrument.Gnirs
  val site       = Site.GN
  val hasFilter  = true
}

// Used for Instruments not fully defined
case class GenericSpectroscopyRow(i: Instrument, grating: String, filter: NonEmptyString)
    extends InstrumentRow {
  type Grating  = String
  type Filter   = NonEmptyString
  type FPU      = Unit
  type Override = Unit
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
            GmosNorthSpectroscopyRow(_, _, _, none)
          )
      case Instrument.GmosSouth  =>
        (decodeGmosSouthGrating(grating0), decodeGmosSouthFPU(fpu0), decodeGmosSouthFilter(filter0))
          .mapN(
            GmosSouthSpectroscopyRow(_, _, _, none)
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

trait ModeCommonWavelengths {
  val λmin: ModeWavelength
  val λmax: ModeWavelength
  val λdelta: WavelengthDelta
}

object ModeCommonWavelengths:
  def wavelengthInterval(
    λ: Wavelength
  ): ModeCommonWavelengths => Option[BoundedInterval[Wavelength]] =
    r =>
      val λr      = r.λdelta
      // Coverage of allowed wavelength
      // Can be simplified once coulomb-refined is available
      val λmin    = r.λmin.value
      val λmax    = r.λmax.value
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

object SlitLength extends NewType[ModeSlitSize] {
  given Order[SlitLength] = Order.by(_.value.value.toMicroarcseconds)
}

type SlitLength = SlitLength.Type

object SlitWidth extends NewType[ModeSlitSize]
type SlitWidth = SlitWidth.Type

case class SpectroscopyModeRow(
  id:         Option[Int], // we number the modes for the UI
  instrument: InstrumentRow,
  config:     NonEmptyString,
  focalPlane: FocalPlane,
  capability: Option[SpectroscopyCapabilities],
  ao:         ModeAO,
  λmin:       ModeWavelength,
  λmax:       ModeWavelength,
  λoptimal:   ModeWavelength,
  λdelta:     WavelengthDelta,
  resolution: PosInt,
  slitLength: SlitLength,
  slitWidth:  SlitWidth
) extends ModeCommonWavelengths {
  // inline def calculatedCoverage: Quantity[NonNegBigDecimal, Micrometer] = wavelengthDelta

  inline def hasFilter: Boolean = instrument.hasFilter

  // This `should` always return a `some`, but if the row is wonky for some reason...
  def intervalCenter(cw: Wavelength): Option[CentralWavelength] =
    ModeCommonWavelengths
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

  val slitWidth: Lens[SpectroscopyModeRow, SlitWidth] =
    GenLens[SpectroscopyModeRow](_.slitWidth)

  val slitLength: Lens[SpectroscopyModeRow, SlitLength] =
    GenLens[SpectroscopyModeRow](_.slitLength)

  def grating: Getter[SpectroscopyModeRow, InstrumentRow#Grating] =
    instrumentRow.andThen(InstrumentRow.grating)

  def fpu: Lens[SpectroscopyModeRow, FocalPlane] =
    GenLens[SpectroscopyModeRow](_.focalPlane)

  def filter: Getter[SpectroscopyModeRow, InstrumentRow#Filter] =
    instrumentRow.andThen(InstrumentRow.filter)

  import lucuma.core.math.units.*

  def resolution: Getter[SpectroscopyModeRow, PosInt] =
    Getter(_.resolution)

  // decodores for instruments are used locally as they are not lawful
  private given Decoder[GmosNorthSpectroscopyRow] = c =>
    for {
      grating <- c.downField("grating").as[GmosNorthGrating]
      fpu     <- c.downField("fpu").as[GmosNorthFpu]
      filter  <- c.downField("filter").as[Option[GmosNorthFilter]]
    } yield GmosNorthSpectroscopyRow(grating, fpu, filter, none)

  private given Decoder[GmosSouthSpectroscopyRow] = c =>
    for {
      grating <- c.downField("grating").as[GmosSouthGrating]
      fpu     <- c.downField("fpu").as[GmosSouthFpu]
      filter  <- c.downField("filter").as[Option[GmosSouthFilter]]
    } yield GmosSouthSpectroscopyRow(grating, fpu, filter, none)

  given Decoder[SpectroscopyModeRow] = c =>
    for {
      instrument <- c.downField("instrument").as[Instrument]
      name       <- c.downField("name").as[NonEmptyString]
      focalPlane <- c.downField("focalPlane").as[FocalPlane]
      capability <- c.downField("capability").as[Option[SpectroscopyCapabilities]]
      ao         <- c.downField("adaptiveOptics").as[Boolean]
      λmin       <- c.downField("wavelengthMin").as[Wavelength]
      λmax       <- c.downField("wavelengthMax").as[Wavelength]
      λoptimal   <- c.downField("wavelengthOptimal").as[Wavelength]
      λcoverage  <- c.downField("wavelengthCoverage").as[Wavelength] // It is wavelength in the odb
      resolution <- c.downField("resolution").as[PosInt]
      slitWidth  <- c.downField("slitWidth").as[Angle]
      slitLength <- c.downField("slitLength").as[Angle]
      gmosNorth  <- c.downField("gmosNorth").as[Option[GmosNorthSpectroscopyRow]]
      gmosSouth  <- c.downField("gmosSouth").as[Option[GmosSouthSpectroscopyRow]]
    } yield gmosNorth
      .orElse(gmosSouth)
      .map { i =>
        SpectroscopyModeRow(
          none,
          i,
          name,
          focalPlane,
          capability,
          ModeAO.fromBoolean(ao),
          ModeWavelength(λmin),
          ModeWavelength(λmax),
          ModeWavelength(λoptimal),
          WavelengthDelta(λcoverage.pm),
          resolution,
          SlitLength(ModeSlitSize(slitLength)),
          SlitWidth(ModeSlitSize(slitWidth))
        )
      }
      .getOrElse(sys.error("Instrument not found"))
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
    slitLength:  Option[SlitLength] = None,
    declination: Option[Declination] = None
  ): List[SpectroscopyModeRow] = {
    // Criteria to filter the modes
    val filter: SpectroscopyModeRow => Boolean = r =>
      focalPlane.forall(f => r.focalPlane === f) &&
        r.capability === capability &&
        iq.forall(i => r.ao =!= ModeAO.AO || (i <= ImageQuality.PointTwo)) &&
        wavelength.forall(w => w >= r.λmin.value && w <= r.λmax.value) &&
        resolution.forall(_ <= r.resolution) &&
        range.forall(_ <= r.λdelta) &&
        slitLength.forall(_ <= r.slitLength) &&
        declination.forall(r.instrument.site.inPreferredDeclination)

    // Calculates a score for each mode for sorting purposes. It is down in Rational space, we may change it to double as we don't really need high precission for this
    val score: SpectroscopyModeRow => Rational = { r =>
      // Difference in wavelength
      val deltaWave: BigDecimal       =
        wavelength
          .map(w => r.λoptimal.value.diff(w).abs.toNanometers.value)
          .getOrElse(BigDecimal(0))
      // Difference in slit width
      val deltaSlitWidth: Rational    =
        iq.map(i =>
          (Rational(r.slitWidth.value.value.toMicroarcseconds, 1000000) - i.toArcSeconds.value).abs
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

object SpectroscopyModesMatrix {
  val empty: SpectroscopyModesMatrix = SpectroscopyModesMatrix(Nil)
}
