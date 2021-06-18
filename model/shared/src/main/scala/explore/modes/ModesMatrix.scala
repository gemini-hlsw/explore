// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Order
import cats.syntax.all._
import coulomb._
import coulomb.refined._
import coulomb.si.Second
import eu.timepit.refined._
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric._
import fs2.data.csv._
import lucuma.core.enum.Instrument
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units._
import lucuma.core.util.Enumerated
import spire.math.Rational

sealed trait ObservationMode extends Product with Serializable

object ObservationMode {
  case object Spectroscopy extends ObservationMode
  case object Imaging      extends ObservationMode
  case object Polarimetry  extends ObservationMode

  /** @group Typeclass Instances */
  implicit val ObservationModeEnumerated: Enumerated[ObservationMode] =
    Enumerated.of(Spectroscopy, Imaging, Polarimetry)
}

case class ModeIQ(iq: Angle) {
  override def toString: String = s"iq(${Angle.milliarcseconds.get(iq)})"
}

object ModeIQ {
  implicit val orderModeIQ: Order[ModeIQ] = Order.by(_.iq.toMicroarcseconds)
}

case class ModeFov(fov: Angle) {
  override def toString: String = s"fov(${Angle.milliarcseconds.get(fov)})"
}

object ModeFov {
  implicit val orderModeFov: Order[ModeFov] = Order.by(_.fov.toMicroarcseconds)
}

case class ModeWavelength(w: Wavelength) {
  override def toString: String = s"wavelength(${w.micrometer.value.toInt})"
}

case class ModeBandWidth(w: Quantity[Rational, Micrometer]) {
  override def toString: String = s"band_width(${w.value.toInt})"
}

case class ModeGratingMinWavelength(w: Wavelength) {
  override def toString: String = s"grcwlen_min(${w.micrometer.value.toInt})"
}

object ModeGratingMinWavelength {
  implicit val orderModeGratingMinWavelength: Order[ModeGratingMinWavelength] = Order.by(_.w)
}

case class ModeGratingMaxWavelength(w: Wavelength) {
  override def toString: String = s"grcwlen_max(${w.micrometer.value.toInt})"
}

object ModeGratingMaxWavelength {
  implicit val orderModeGratingMaxWavelength: Order[ModeGratingMaxWavelength] = Order.by(_.w)
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
  mode:                 ObservationMode,
  fov:                  ModeFov,
  iqMin:                ModeIQ,
  iqMax:                ModeIQ,
  resolution:           PosBigDecimal,
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
  mos:                  ModeMOS,
  minExposure:          Quantity[PosBigDecimal, Second]
)

trait ModesMatrixDecoders {
  implicit val instDecoder: CellDecoder[Instrument] =
    CellDecoder.stringDecoder
      .emap {
        case "FLAMINGOS2" => Instrument.Flamingos2.asRight
        case "GSAOI"      => Instrument.Gsaoi.asRight
        case "GMOS-S"     => Instrument.GmosSouth.asRight
        case "GMOS-N"     => Instrument.GmosNorth.asRight
        case "GPI"        => Instrument.Gpi.asRight
        case "NIFS"       => Instrument.Nifs.asRight
        case x            => new DecoderError(s"Unknown instrument $x").asLeft
      }

  implicit val instModeDecoder: CellDecoder[ObservationMode] =
    CellDecoder.stringDecoder
      .emap {
        case "spec"        => ObservationMode.Spectroscopy.asRight
        case "imaging"     => ObservationMode.Imaging.asRight
        case "polarimetry" => ObservationMode.Polarimetry.asRight
        case x             => new DecoderError(s"Unknown instrument mode $x").asLeft
      }

  val arcsecDecoder: CellDecoder[Angle] =
    CellDecoder.bigDecimalDecoder.map(x => Angle.milliarcseconds.reverseGet((x * 1000).intValue))

  implicit val iqDecoder: CellDecoder[ModeIQ] =
    arcsecDecoder.map(ModeIQ.apply)

  implicit val fovDecoder: CellDecoder[ModeFov] =
    arcsecDecoder.map(ModeFov.apply)

  val micrometerDecoder: CellDecoder[Wavelength] =
    CellDecoder.bigDecimalDecoder.emap(x =>
      Wavelength.fromPicometers
        .getOption((x * 1000000).intValue)
        .toRight(new DecoderError(s"Invalid wavelength value $x"))
    )

  implicit val wavelength: CellDecoder[ModeWavelength] =
    micrometerDecoder.map(ModeWavelength.apply)

  implicit val bandWidth: CellDecoder[ModeBandWidth] =
    micrometerDecoder.map(w => ModeBandWidth(w.nanometer))

  implicit val gratingMinWv: CellDecoder[ModeGratingMinWavelength] =
    micrometerDecoder.map(ModeGratingMinWavelength.apply)

  implicit val gratingMaxWv: CellDecoder[ModeGratingMaxWavelength] =
    micrometerDecoder.map(ModeGratingMaxWavelength.apply)

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

  implicit val posBigDecimalDecoder: CellDecoder[PosBigDecimal] =
    CellDecoder.bigDecimalDecoder
      .emap { x =>
        refineV[Positive](x).leftMap(s => new DecoderError(s))
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

  implicit val modeMOSDecoder: CellDecoder[ModeMOS] =
    CellDecoder.stringDecoder
      .map {
        case "yes" => ModeMOS.MOS
        case _     => ModeMOS.NoMOS
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
        m    <- row.as[ObservationMode]("mode")
        f    <- row.as[ModeFov]("fov")
        im   <- row.as[ModeIQ]("iq_min")
        ix   <- row.as[ModeIQ]("iq_max")
        r    <- row.as[PosBigDecimal]("resolution")
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
        mo   <- row.as[ModeMOS]("mos")
        me   <- row.as[Quantity[PosBigDecimal, Second]]("minexp")
      } yield ModeRow(i, m, f, im, ix, r, w, b, gmin, gmax, mf, sw, ao, sd, c, ss, mo, me)
  }
}

trait ModesMatrix[F[_]] {
  def matrix: List[ModeRow]

  val DefaultMinExp: Quantity[PosBigDecimal, Second] =
    BigDecimal(1).withRefinedUnit[Positive, Second]

  def spectroscopyModes(
    dwmin:       Option[ModeBandWidth],
    dwmax:       Option[ModeBandWidth],
    rmin:        Option[PosBigDecimal],
    dims:        Option[PosInt],
    coronograph: Option[ModeCoronagraph],
    mexp:        Option[Quantity[PosBigDecimal, Second]],
    mos:         Option[ModeMOS],
    skysub:      Option[ModeSkysub],
    iqmax:       Option[ModeIQ],
    fov:         Option[ModeFov],
    wlen:        Option[Wavelength]
  ): List[ModeRow] = {
    val defaultIQMax                 = ModeIQ(Angle.fromDoubleArcseconds(10))
    val defaultFOV                   = ModeFov(Angle.fromDoubleArcseconds(1))
    val l_dwmin                      = dwmin.map(_.w).getOrElse(Rational.zero.withUnit[Micrometer])
    val l_dwmax                      = dwmax.map(_.w).getOrElse(Rational(10).withUnit[Micrometer])
    val criteria: ModeRow => Boolean = m =>
      m.mode === ObservationMode.Spectroscopy &&
        rmin.forall(m.resolution >= _) &&
        dims.forall(m.spatialDimensions >= _) &&
        m.bandWidth.w >= l_dwmin &&
        m.bandWidth.w <= l_dwmax &&
        coronograph.forall(_ === m.coronograph) &&
        m.minExposure <= mexp.getOrElse(DefaultMinExp) &&
        mos.forall(_ === m.mos) &&
        skysub.forall(_ === m.skySub) &&
        m.iqMin <= iqmax.getOrElse(defaultIQMax) &&
        m.fov >= fov.getOrElse(defaultFOV) &&
        wlen.forall(m.gratingMinWavelength.w <= _) &&
        wlen.forall(m.gratingMaxWavelength.w >= _)

    matrix.filter(criteria)
  }
}

object ModesMatrix extends ModesMatrixPlatform
