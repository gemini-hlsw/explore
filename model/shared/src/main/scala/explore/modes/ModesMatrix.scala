// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Eq
import cats.Order
import cats.Order._
import cats.derived.*
import cats.syntax.all._
import coulomb._
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.si.Second
import eu.timepit.refined._
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric._
import fs2.data.csv._
import lucuma.core.enums.Instrument
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units._
import lucuma.core.util.Enumerated
import monocle.Lens
import monocle.macros.GenLens
import spire.math.Rational

enum ObservationMode derives Eq:
  case Spectroscopy, Imaging, Polarimetry

case class ModeIQ(iq: Angle) {
  override def toString: String = s"iq(${Angle.milliarcseconds.get(iq)})"
}

object ModeIQ {
  given Order[ModeIQ] = Order.by(_.iq.toMicroarcseconds)
}

case class ModeFov(fov: Angle) {
  override def toString: String = s"fov(${Angle.milliarcseconds.get(fov)})"
}

object ModeFov {
  given Order[ModeFov] = Order.by(_.fov.toMicroarcseconds)
}

case class ModeBandWidth(w: Quantity[Rational, Micrometer]) {
  override def toString: String = s"band_width(${w.value.toInt})"
}

case class ModeGratingMinWavelength(w: Wavelength) {
  override def toString: String = s"grcwlen_min(${w.micrometer.value.toInt})"
}

object ModeGratingMinWavelength {
  given Order[ModeGratingMinWavelength] = Order.by(_.w)
}

case class ModeGratingMaxWavelength(w: Wavelength) {
  override def toString: String = s"grcwlen_max(${w.micrometer.value.toInt})"
}

object ModeGratingMaxWavelength {
  given Order[ModeGratingMaxWavelength] = Order.by(_.w)
}

enum ModeFilter:
  // At the moment we only care about the presence of filter
  case NoFilter, SomeFilter

sealed trait ModeGrating extends Product with Serializable

object ModeGrating {
  // At the moment we only care about the presence of filter
  case object NoGrating               extends ModeGrating
  case class SomeGrating(tag: String) extends ModeGrating
}

enum ModeSpatialDimension(val pos: Int):
  case One extends ModeSpatialDimension(1)
  case Two extends ModeSpatialDimension(2)

object ModeSpatialDimension:
  /** @group Typeclass Instances */
  given Order[ModeSpatialDimension] = Order.by(_.pos)

enum ModeCoronagraph derives Eq:
  case NoCoronagraph, Coronagraph

enum ModeSkysub derives Eq:
  case Normal, High

enum ModeMOS derives Eq:
  case NoMOS, MOS

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
  grating:              ModeGrating,
  slitWidth:            ModeSlitSize,
  ao:                   ModeAO,
  spatialDimensions:    ModeSpatialDimension,
  coronograph:          ModeCoronagraph,
  skySub:               ModeSkysub,
  mos:                  ModeMOS,
  minExposure:          Quantity[PosBigDecimal, Second]
)

object ModeRow {
  val instrument: Lens[ModeRow, Instrument] = GenLens[ModeRow](_.instrument)
  val grating: Lens[ModeRow, ModeGrating]   = GenLens[ModeRow](_.grating)
}

trait ModesMatrixDecoders extends Decoders {

  given CellDecoder[ObservationMode] =
    CellDecoder.stringDecoder
      .emap {
        case "spec"        => ObservationMode.Spectroscopy.asRight
        case "imaging"     => ObservationMode.Imaging.asRight
        case "polarimetry" => ObservationMode.Polarimetry.asRight
        case x             => new DecoderError(s"Unknown instrument mode $x").asLeft
      }

  given CellDecoder[ModeIQ] =
    arcsecDecoder.map(ModeIQ.apply)

  given CellDecoder[ModeFov] =
    arcsecDecoder.map(ModeFov.apply)

  given CellDecoder[ModeBandWidth] =
    micrometerDecoder.map(w => ModeBandWidth(w.nanometer))

  given CellDecoder[ModeGratingMinWavelength] =
    micrometerDecoder.map(ModeGratingMinWavelength.apply)

  given CellDecoder[ModeGratingMaxWavelength] =
    micrometerDecoder.map(ModeGratingMaxWavelength.apply)

  given CellDecoder[ModeFilter] =
    CellDecoder.stringDecoder
      .map {
        case "none" => ModeFilter.NoFilter
        case _      => ModeFilter.SomeFilter
      }

  given CellDecoder[ModeGrating] =
    CellDecoder.stringDecoder
      .map {
        case "none" => ModeGrating.NoGrating
        case x      => ModeGrating.SomeGrating(x)
      }

  given CellDecoder[ModeSpatialDimension] =
    CellDecoder.intDecoder
      .emap {
        case 1 => ModeSpatialDimension.One.asRight
        case 2 => ModeSpatialDimension.Two.asRight
        case x => new DecoderError(s"Unsupported spatial dimensions $x").asLeft
      }

  given CellDecoder[ModeCoronagraph] =
    CellDecoder.stringDecoder
      .map {
        case "yes" => ModeCoronagraph.Coronagraph
        case _     => ModeCoronagraph.NoCoronagraph
      }

  given CellDecoder[ModeSkysub] =
    CellDecoder.stringDecoder
      .emap {
        case "normal" => ModeSkysub.Normal.asRight
        case "high"   => ModeSkysub.High.asRight
        case x        => new DecoderError(s"Unknwon mos mode $x").asLeft
      }

  given CellDecoder[ModeMOS] =
    CellDecoder.stringDecoder
      .map {
        case "yes" => ModeMOS.MOS
        case _     => ModeMOS.NoMOS
      }

  given CellDecoder[Quantity[PosBigDecimal, Second]] =
    CellDecoder.bigDecimalDecoder
      .emap { x =>
        refineV[Positive](x).bimap(s => new DecoderError(s), _.withUnit[Second])
      }

  given CsvRowDecoder[ModeRow, String] = (row: CsvRow[String]) =>
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
      di   <- row.as[ModeGrating]("disperser")
      sw   <- row.as[ModeSlitSize]("slit_width")
      ao   <- row.as[ModeAO]("ao")
      sd   <- row.as[ModeSpatialDimension]("spatial_dims")
      c    <- row.as[ModeCoronagraph]("coronagraph")
      ss   <- row.as[ModeSkysub]("skysub")
      mo   <- row.as[ModeMOS]("mos")
      me   <- row.as[Quantity[PosBigDecimal, Second]]("minexp")
    } yield ModeRow(i, m, f, im, ix, r, w, b, gmin, gmax, mf, di, sw, ao, sd, c, ss, mo, me)
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
