// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Eq
import cats.Order
import cats.data.NonEmptyList
import cats.derived.*
import cats.implicits.*
import coulomb.*
import coulomb.conversion.ValueConversion
import eu.timepit.refined.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.*
import eu.timepit.refined.types.numeric.*
import eu.timepit.refined.types.string.*
import explore.model.syntax.all.*
import io.circe.Decoder
import io.circe.refined.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.BoundedInterval
import lucuma.core.math.BoundedInterval.*
import lucuma.core.math.Declination
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.longslit.DefaultRoi
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.wavelength.decoder.given
import lucuma.schemas.model.CentralWavelength
import monocle.Getter
import monocle.Lens
import monocle.macros.GenLens
import spire.math.Rational

trait ModeCommonWavelengths {
  val λmin: ModeWavelength
  val λmax: ModeWavelength
  val λdelta: WavelengthDelta

  def wavelengthInterval(
    λ: Wavelength
  ): Option[BoundedInterval[Wavelength]] =
    val λr      = λdelta
    // Coverage of allowed wavelength
    // Can be simplified once coulomb-refined is available
    val λminV   = λmin.value
    val λmaxV   = λmax.value
    val range   = λr.centeredAt(λ)
    // if we are below min clip but shift the range
    // same if we are above max
    // At any event we clip at min/max
    val shifted =
      if (range.lower < λminV)
        λr.startingAt(λminV)
      else if (range.upper > λmaxV)
        λr.endingAt(λmaxV)
      else
        range
    shifted.intersect(BoundedInterval.unsafeClosed(λminV, λmaxV))
}

object SlitLength extends NewType[ModeSlitSize] {
  given Order[SlitLength] = Order.by(_.value.value.toMicroarcseconds)
}

type SlitLength = SlitLength.Type

object SlitWidth extends NewType[ModeSlitSize]
type SlitWidth = SlitWidth.Type

case class SpectroscopyModeRow(
  id:         Option[Int], // we number the modes for the UI
  instrument: ItcInstrumentConfig,
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
) extends ModeCommonWavelengths derives Eq {
  inline def hasFilter: Boolean = instrument.hasFilter

  // This `should` always return a `some`, but if the row is wonky for some reason...
  def intervalCenter(cw: Wavelength): Option[CentralWavelength] =
    range(cw)
      .map: interval =>
        interval.lower.pm.value.value + (interval.upper.pm.value.value - interval.lower.pm.value.value) / 2
      .flatMap(pms => Wavelength.fromIntPicometers(pms))
      .map(CentralWavelength(_))

  // Range of wavelengths for this row. It takes the min/max and adjust it for the requested
  // wavelength
  def range(cw: Wavelength): Option[BoundedInterval[Wavelength]] =
    wavelengthInterval(cw)

  def withModeOverridesFor(
    wavelength:   Option[Wavelength],
    profiles:     Option[NonEmptyList[SourceProfile]],
    imageQuality: ImageQuality.Preset
  ): Option[SpectroscopyModeRow] =
    (wavelength, profiles)
      .flatMapN { (w, p) =>
        withModeOverridesFor(w, p, imageQuality)
      }
      .orElse(this.some)

  private def withModeOverridesFor(
    wavelength:   Wavelength,
    profiles:     NonEmptyList[SourceProfile],
    imageQuality: ImageQuality.Preset
  ): Option[SpectroscopyModeRow] =
    intervalCenter(wavelength).flatMap: cw =>
      val instrumentConfig: Option[ItcInstrumentConfig] =
        instrument.instrument match
          case Instrument.GmosNorth | Instrument.GmosSouth | Instrument.Flamingos2 =>
            // In case we have no particular overrides, set ccd with binning calculated
            // from the target
            instrument match
              case i @ ItcInstrumentConfig.GmosNorthSpectroscopy(grating, fpu, _, None) =>
                i.copy(modeOverrides =
                  InstrumentOverrides
                    .GmosSpectroscopy(
                      cw,
                      GmosCcdMode.Default.Longslit
                        .gmosNorth(profiles, fpu, grating, imageQuality.toImageQuality),
                      DefaultRoi
                    )
                    .some
                ).some
              case i @ ItcInstrumentConfig.GmosSouthSpectroscopy(grating, fpu, _, None) =>
                i.copy(modeOverrides =
                  InstrumentOverrides
                    .GmosSpectroscopy(
                      cw,
                      GmosCcdMode.Default.Longslit
                        .gmosSouth(profiles, fpu, grating, imageQuality.toImageQuality),
                      DefaultRoi
                    )
                    .some
                ).some
              case i @ ItcInstrumentConfig.Flamingos2Spectroscopy(_, _, _)              =>
                i.some
              case i                                                                    =>
                i.some
          case _                                                                   => none

      instrumentConfig.map: i =>
        copy(instrument = i)
}

object SpectroscopyModeRow {

  given ValueConversion[NonNegBigDecimal, BigDecimal] = _.value

  val instrumentConfig: Lens[SpectroscopyModeRow, ItcInstrumentConfig] =
    GenLens[SpectroscopyModeRow](_.instrument)

  val instrument: Getter[SpectroscopyModeRow, Instrument] =
    instrumentConfig.andThen(ItcInstrumentConfig.instrument)

  val config: Lens[SpectroscopyModeRow, NonEmptyString] =
    GenLens[SpectroscopyModeRow](_.config)

  val instrumentAndConfig: Getter[SpectroscopyModeRow, (Instrument, NonEmptyString)] =
    instrument.zip(config.asGetter)

  val slitWidth: Lens[SpectroscopyModeRow, SlitWidth] =
    GenLens[SpectroscopyModeRow](_.slitWidth)

  val slitLength: Lens[SpectroscopyModeRow, SlitLength] =
    GenLens[SpectroscopyModeRow](_.slitLength)

  def grating: Getter[SpectroscopyModeRow, ItcInstrumentConfig#Grating] =
    instrumentConfig.andThen(ItcInstrumentConfig.grating)

  def fpu: Lens[SpectroscopyModeRow, FocalPlane] =
    GenLens[SpectroscopyModeRow](_.focalPlane)

  def filter: Getter[SpectroscopyModeRow, ItcInstrumentConfig#Filter] =
    instrumentConfig.andThen(ItcInstrumentConfig.filter)

  def resolution: Getter[SpectroscopyModeRow, PosInt] =
    Getter(_.resolution)

  // decoders for instruments are used locally as they are not lawful
  private given Decoder[ItcInstrumentConfig.GmosNorthSpectroscopy] = c =>
    for {
      grating <- c.downField("grating").as[GmosNorthGrating]
      fpu     <- c.downField("fpu").as[GmosNorthFpu]
      filter  <- c.downField("filter").as[Option[GmosNorthFilter]]
    } yield ItcInstrumentConfig.GmosNorthSpectroscopy(grating, fpu, filter, none)

  private given Decoder[ItcInstrumentConfig.GmosSouthSpectroscopy] = c =>
    for {
      grating <- c.downField("grating").as[GmosSouthGrating]
      fpu     <- c.downField("fpu").as[GmosSouthFpu]
      filter  <- c.downField("filter").as[Option[GmosSouthFilter]]
    } yield ItcInstrumentConfig.GmosSouthSpectroscopy(grating, fpu, filter, none)

  private given Decoder[ItcInstrumentConfig.Flamingos2Spectroscopy] = c =>
    for {
      disperser <- c.downField("disperser").as[F2Disperser]
      filter    <- c.downField("filter").as[F2Filter]
      fpu       <- c.downField("fpu").as[F2Fpu]
    } yield ItcInstrumentConfig.Flamingos2Spectroscopy(disperser, filter, fpu)

  given Decoder[SpectroscopyModeRow] = c =>
    for {
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
      gmosNorth  <- c.downField("gmosNorth").as[Option[ItcInstrumentConfig.GmosNorthSpectroscopy]]
      gmosSouth  <- c.downField("gmosSouth").as[Option[ItcInstrumentConfig.GmosSouthSpectroscopy]]
      flamingos2 <- c.downField("flamingos2").as[Option[ItcInstrumentConfig.Flamingos2Spectroscopy]]
    } yield gmosNorth
      .orElse(gmosSouth)
      .orElse(flamingos2)
      .map { i =>
        SpectroscopyModeRow(
          none,
          i,
          name,
          focalPlane,
          capability,
          ModeAO(ao),
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

case class SpectroscopyModesMatrix(matrix: List[SpectroscopyModeRow]) derives Eq {
  val ScoreBump   = Rational(1, 2)
  val FilterLimit = Wavelength.fromIntNanometers(650)

  def filtered(
    focalPlane:  Option[FocalPlane] = None,
    capability:  Option[SpectroscopyCapabilities] = None,
    iq:          Option[ImageQuality.Preset] = None,
    wavelength:  Option[Wavelength] = None,
    resolution:  Option[PosInt] = None,
    range:       Option[WavelengthDelta] = None,
    slitLength:  Option[SlitLength] = None,
    declination: Option[Declination] = None
  ): List[SpectroscopyModeRow] = {
    // Criteria to filter the modes
    val filter: SpectroscopyModeRow => Boolean = r =>
      // contains the  range in picometers
      val rowRangePM: Option[Int] =
        wavelength.flatMap(r.range(_).map(w => w.upper.pm.value - w.lower.pm.value))

      focalPlane.forall(f => r.focalPlane === f) &&
      r.capability === capability &&
      iq.forall(i => r.ao =!= ModeAO.AO || (i <= ImageQuality.Preset.PointTwo)) &&
      wavelength.forall(w => w >= r.λmin.value && w <= r.λmax.value) &&
      resolution.forall(_ <= r.resolution) &&
      (range, rowRangePM).mapN(_.pm.value <= _).forall(identity) &&
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
          (Rational(r.slitWidth.value.value.toMicroarcseconds, 1000000) -
            i.toImageQuality.toArcSeconds).abs
        ).getOrElse(Rational.zero)
      // Difference in resolution
      val deltaRes: BigDecimal        =
        resolution.foldMap(re => (re.value - r.resolution.value).abs)
      // give a bumpp to non-AO modes (but don't discard them)
      val aoScore: Rational           =
        if (iq.forall(i => r.ao =!= ModeAO.AO || (i <= ImageQuality.Preset.PointTwo))) ScoreBump
        else Rational.zero
      // If wavelength > 0.65mu, then prefer settings with a filter to avoid 2nd order contamination
      val filterScore: Rational       =
        (wavelength, FilterLimit)
          .mapN { (w, l) =>
            if (w >= l && r.hasFilter) ScoreBump else Rational.zero
          }
          .getOrElse(Rational.zero)
      // Wavelength match
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
          iq.map(i =>
            Rational(
              i.toImageQuality.toArcSeconds / (i.toImageQuality.toArcSeconds + deltaSlitWidth)
            )
          ).getOrElse(Rational.zero)
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
