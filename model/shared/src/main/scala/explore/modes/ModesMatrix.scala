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
import monocle.Lens
import monocle.macros.GenLens
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

sealed trait ModeDisperser extends Product with Serializable

object ModeDisperser {
  // At the moment we only care about the presence of filter
  case object NoDisperser extends ModeDisperser
  case class SomeDisperser(tag: String) extends ModeDisperser
}

sealed trait ModeSpatialDimension extends Product with Serializable

object ModeSpatialDimension {
  case object One extends ModeSpatialDimension
  case object Two extends ModeSpatialDimension

  /** @group Typeclass Instances */
  implicit val ModeSpatialDimensionEnumerated: Enumerated[ModeSpatialDimension] =
    Enumerated.of(One, Two)
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
  disperser:            ModeDisperser,
  slitWidth:            ModeSlitSize,
  ao:                   ModeAO,
  spatialDimensions:    ModeSpatialDimension,
  coronograph:          ModeCoronagraph,
  skySub:               ModeSkysub,
  mos:                  ModeMOS,
  minExposure:          Quantity[PosBigDecimal, Second]
)

object ModeRow {
  val instrument: Lens[ModeRow, Instrument]   = GenLens[ModeRow](_.instrument)
  val disperser: Lens[ModeRow, ModeDisperser] = GenLens[ModeRow](_.disperser)
}

trait ModesMatrixDecoders extends Decoders {

  implicit val instModeDecoder: CellDecoder[ObservationMode] =
    CellDecoder.stringDecoder
      .emap {
        case "spec"        => ObservationMode.Spectroscopy.asRight
        case "imaging"     => ObservationMode.Imaging.asRight
        case "polarimetry" => ObservationMode.Polarimetry.asRight
        case x             => new DecoderError(s"Unknown instrument mode $x").asLeft
      }

  implicit val iqDecoder: CellDecoder[ModeIQ] =
    arcsecDecoder.map(ModeIQ.apply)

  implicit val fovDecoder: CellDecoder[ModeFov] =
    arcsecDecoder.map(ModeFov.apply)

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

  implicit val modeDisperser: CellDecoder[ModeDisperser] =
    CellDecoder.stringDecoder
      .map {
        case "none" => ModeDisperser.NoDisperser
        case x      => ModeDisperser.SomeDisperser(x)
      }

  implicit val modeSpatialDimensionDecoder: CellDecoder[ModeSpatialDimension] =
    CellDecoder.intDecoder
      .emap {
        case 1 => ModeSpatialDimension.One.asRight
        case 2 => ModeSpatialDimension.Two.asRight
        case x => new DecoderError(s"Unsupported spatial dimensions $x").asLeft
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
        di   <- row.as[ModeDisperser]("disperser")
        sw   <- row.as[ModeSlitSize]("slit_width")
        ao   <- row.as[ModeAO]("ao")
        sd   <- row.as[ModeSpatialDimension]("spatial_dims")
        c    <- row.as[ModeCoronagraph]("coronagraph")
        ss   <- row.as[ModeSkysub]("skysub")
        mo   <- row.as[ModeMOS]("mos")
        me   <- row.as[Quantity[PosBigDecimal, Second]]("minexp")
      } yield ModeRow(i, m, f, im, ix, r, w, b, gmin, gmax, mf, di, sw, ao, sd, c, ss, mo, me)
  }
}

final case class ModesMatrix(matrix: List[ModeRow]) {

  val DefaultMinExp: Quantity[PosBigDecimal, Second] =
    BigDecimal(1).withRefinedUnit[Positive, Second]

  def spectroscopyModes(
    dwmin:       Option[ModeBandWidth],
    dwmax:       Option[ModeBandWidth],
    rmin:        Option[PosBigDecimal],
    dims:        Option[ModeSpatialDimension],
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

object ModesMatrix extends ModesMatrixPlatform {
  val empty: ModesMatrix = ModesMatrix(Nil)
}
