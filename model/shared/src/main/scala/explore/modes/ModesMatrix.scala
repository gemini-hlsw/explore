package explore.modes

import cats.syntax.all._
import coulomb._
import coulomb.si.Second
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric._
import fs2.data.csv._
import lucuma.core.enum.Instrument
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

sealed trait InstrumentMode extends Product with Serializable

object InstrumentMode {
  case object Spectroscopy extends InstrumentMode
  case object Imaging      extends InstrumentMode
  case object Polarimetry  extends InstrumentMode

  /** @group Typeclass Instances */
  implicit val InstrumentModeEnumerated: Enumerated[InstrumentMode] =
    Enumerated.of(Spectroscopy, Imaging, Polarimetry)
}

case class ModeIQ(iq: Angle) {
  override def toString: String = s"iq(${Angle.milliarcseconds.get(iq)})"
}

case class ModeFov(fov: Angle) {
  override def toString: String = s"fov(${Angle.milliarcseconds.get(fov)})"
}

case class ModeWavelength(w: Wavelength) {
  override def toString: String = s"wavelength(${Wavelength.decimalNanometers.reverseGet(w)})"
}

case class ModeBandWidth(w: Wavelength) {
  override def toString: String = s"band_width(${Wavelength.decimalNanometers.reverseGet(w)})"
}

case class ModeGratingMinWavelength(w: Wavelength) {
  override def toString: String = s"grcwlen_min(${Wavelength.decimalNanometers.reverseGet(w)})"
}

case class ModeGratingMaxWavelength(w: Wavelength) {
  override def toString: String = s"grcwlen_max(${Wavelength.decimalNanometers.reverseGet(w)})"
}

sealed trait ModeFilter extends Product with Serializable

object ModeFilter {
  // At the moment we only care about the presence of filter
  case object NoFilter   extends ModeFilter
  case object SomeFilter extends ModeFilter

  /** @group Typeclass Instances */
  implicit val ModeFilterEnumerated: Enumerated[ModeFilter] =
    Enumerated.of(NoFilter, SomeFilter)
}

case class ModeSlitWidth(sw: Angle) {
  override def toString: String = s"slit_width(${Angle.milliarcseconds.get(sw)})"
}

sealed trait ModeAO extends Product with Serializable

object ModeAO {
  case object NoAO extends ModeAO
  case object AO   extends ModeAO

  /** @group Typeclass Instances */
  implicit val ModeAOEnumerated: Enumerated[ModeAO] =
    Enumerated.of(NoAO, AO)
}

sealed trait ModeCoronagraph extends Product with Serializable

object ModeCoronagraph {
  case object NoCoronagraph extends ModeCoronagraph
  case object Coronagraph   extends ModeCoronagraph

  /** @group Typeclass Instances */
  implicit val ModeCoronagraphEnumerated: Enumerated[ModeCoronagraph] =
    Enumerated.of(NoCoronagraph, Coronagraph)
}

sealed trait ModeSkysub extends Product with Serializable

object ModeSkysub {
  case object Normal extends ModeSkysub
  case object High   extends ModeSkysub

  /** @group Typeclass Instances */
  implicit val ModeSkysubEnumerated: Enumerated[ModeSkysub] =
    Enumerated.of(Normal, High)
}

sealed trait ModeMOS extends Product with Serializable

object ModeMOS {
  case object NoMOS extends ModeMOS
  case object MOS   extends ModeMOS

  /** @group Typeclass Instances */
  implicit val ModeMOSEnumerated: Enumerated[ModeMOS] =
    Enumerated.of(NoMOS, MOS)
}

case class ModeRow(
  instrument:           Instrument,
  mode:                 InstrumentMode,
  fov:                  ModeFov,
  iqMin:                ModeIQ,
  iqMax:                ModeIQ,
  resolution:           BigDecimal,
  wavelength:           ModeWavelength,
  bandWidth:            ModeBandWidth,
  gratingMinWavelength: ModeGratingMinWavelength,
  gratingMaxWavelength: ModeGratingMaxWavelength,
  filter:               ModeFilter,
  slitWidth:            ModeSlitWidth,
  ao:                   ModeAO,
  spatialDimensions:    PosInt,
  coronograph:          ModeCoronagraph,
  skySub:               ModeSkysub,
  minExposure:          Quantity[PosBigDecimal, Second]
)

trait ModesMatrixDecoders {
  implicit val instDecoder: CellDecoder[Instrument] =
    CellDecoder.stringDecoder
      .emap {
        case "FLAMINGOS2" => Instrument.Flamingos2.asRight
        case "GSAOI"      => Instrument.Gsaoi.asRight
        case "GMOS-S"     => Instrument.Gsaoi.asRight
        case "GMOS-N"     => Instrument.GmosN.asRight
        case "GPI"        => Instrument.Gpi.asRight
        case "NIFS"       => Instrument.Nifs.asRight
        case x            => new DecoderError(s"Unknown instrument $x").asLeft
      }

  implicit val instModeDecoder: CellDecoder[InstrumentMode] =
    CellDecoder.stringDecoder
      .emap {
        case "spec"        => InstrumentMode.Spectroscopy.asRight
        case "imaging"     => InstrumentMode.Imaging.asRight
        case "polarimetry" => InstrumentMode.Polarimetry.asRight
        case x             => new DecoderError(s"Unknown instrument mode $x").asLeft
      }

  val arcsecDecoder: CellDecoder[Angle] =
    CellDecoder.bigDecimalDecoder.map(x => Angle.milliarcseconds.reverseGet((x * 1000).intValue))

  implicit val iqDecoder: CellDecoder[ModeIQ] =
    arcsecDecoder.map(ModeIQ.apply)

  implicit val fovDecoder: CellDecoder[ModeFov] =
    arcsecDecoder.map(ModeFov.apply)

  val nanometerDecoder: CellDecoder[Wavelength] =
    CellDecoder.bigDecimalDecoder.emap(x =>
      Wavelength.fromPicometers
        .getOption((x * 1000).intValue)
        .toRight(new DecoderError(s"Invalid wavelength value $x"))
    )

  implicit val wavelength: CellDecoder[ModeWavelength] =
    nanometerDecoder.map(ModeWavelength.apply)

  implicit val bandWidth: CellDecoder[ModeBandWidth] =
    nanometerDecoder.map(ModeBandWidth.apply)

  implicit val gratingMinWv: CellDecoder[ModeGratingMinWavelength] =
    nanometerDecoder.map(ModeGratingMinWavelength.apply)

  implicit val gratingMaxWv: CellDecoder[ModeGratingMaxWavelength] =
    nanometerDecoder.map(ModeGratingMaxWavelength.apply)

  implicit val modeFilter: CellDecoder[ModeFilter] =
    CellDecoder.stringDecoder
      .map {
        case "none" => ModeFilter.NoFilter
        case _      => ModeFilter.SomeFilter
      }

  implicit val swDecoder: CellDecoder[ModeSlitWidth] =
    arcsecDecoder.map(ModeSlitWidth.apply)

  implicit val modeAODecoder: CellDecoder[ModeAO] =
    CellDecoder.stringDecoder
      .map {
        case "yes" => ModeAO.AO
        case _     => ModeAO.NoAO
      }

  implicit val posIntDecoder: CellDecoder[PosInt] =
    CellDecoder.intDecoder
      .emap { x =>
        refineV[Positive](x).leftMap(s => new DecoderError(s))
      }

  implicit val modeCoronagraphDecoder: CellDecoder[ModeCoronagraph] =
    CellDecoder.stringDecoder
      .map {
        case "yes" => ModeCoronagraph.Coronagraph
        case _     => ModeCoronagraph.NoCoronagraph
      }

  implicit val modeSkysubDecoder: CellDecoder[ModeSkysub] =
    CellDecoder.stringDecoder
      .emap {
        case "normal" => ModeSkysub.Normal.asRight
        case "high"   => ModeSkysub.High.asRight
        case x        => new DecoderError(s"Unknwon mos mode $x").asLeft
      }

  implicit val modeMinExpDecoder: CellDecoder[Quantity[PosBigDecimal, Second]] =
    CellDecoder.bigDecimalDecoder
      .emap { x =>
        refineV[Positive](x).bimap(s => new DecoderError(s), _.withUnit[Second])
      }

  implicit object ModeRowDecoder extends CsvRowDecoder[ModeRow, String] {
    def apply(row: CsvRow[String]): DecoderResult[ModeRow] =
      for {
        i    <- row.as[Instrument]("instrument")
        m    <- row.as[InstrumentMode]("mode")
        f    <- row.as[ModeFov]("fov")
        im   <- row.as[ModeIQ]("iq_min")
        ix   <- row.as[ModeIQ]("iq_max")
        r    <- row.as[BigDecimal]("resolution")
        w    <- row.as[ModeWavelength]("wavelength")
        b    <- row.as[ModeBandWidth]("band_width")
        gmin <- row.as[ModeGratingMinWavelength]("grcwlen_min")
        gmax <- row.as[ModeGratingMaxWavelength]("grcwlen_max")
        mf   <- row.as[ModeFilter]("filter")
        sw   <- row.as[ModeSlitWidth]("slit_width")
        ao   <- row.as[ModeAO]("ao")
        sd   <- row.as[PosInt]("spatial_dims")
        c    <- row.as[ModeCoronagraph]("coronagraph")
        ss   <- row.as[ModeSkysub]("skysub")
        me   <- row.as[Quantity[PosBigDecimal, Second]]("minexp")
      } yield ModeRow(i, m, f, im, ix, r, w, b, gmin, gmax, mf, sw, ao, sd, c, ss, me)
  }
}

trait ModesMatrix[F[_]] {
  def matrix: List[ModeRow]
}

object ModesMatrix extends ModesMatrixPlatform
