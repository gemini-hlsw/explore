// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Eq
import cats.Order
import cats.derived.*
import cats.syntax.all.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.si.Second
import eu.timepit.refined.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.*
import fs2.data.csv.*
import lucuma.core.enums.Instrument
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units.*
import lucuma.core.math.units.given
import lucuma.core.util.NewType
import monocle.Lens
import monocle.macros.GenLens

enum ObservationMode derives Order:
  case Spectroscopy, Imaging, Polarimetry

object ModeIQ extends NewType[Angle] {
  given Order[ModeIQ] = Order.by(_.value.toMicroarcseconds)
}
type ModeIQ = ModeIQ.Type

object ModeFov extends NewType[Angle] {
  given Order[ModeFov] = Order.by(_.value.toMicroarcseconds)
}
type ModeFov = ModeFov.Type

object ModeBandwidth extends NewType[Quantity[BigDecimal, Micrometer]] {
  val Zero                   = ModeBandwidth(BigDecimal(0).withUnit[Micrometer])
  val Ten                    = ModeBandwidth(BigDecimal(10).withUnit[Micrometer])
  given Order[ModeBandwidth] = Order.by(_.value.value)
}

type ModeBandwidth = ModeBandwidth.Type

object ModeGratingMinWavelength extends NewType[Wavelength] {
  given Order[ModeGratingMinWavelength] = Order.by(_.value)
}
type ModeGratingMinWavelength = ModeGratingMinWavelength.Type

object ModeGratingMaxWavelength extends NewType[Wavelength] {
  given Order[ModeGratingMinWavelength] = Order.by(_.value)
}
type ModeGratingMaxWavelength = ModeGratingMaxWavelength.Type

enum ModeFilter derives Order:
  // At the moment we only care about the presence of filter
  case NoFilter, SomeFilter

enum ModeGrating derives Order:
  // At the moment we only care about the presence of grating
  case NoGrating                extends ModeGrating
  case SomeGrating(tag: String) extends ModeGrating

enum ModeSpatialDimension derives Order:
  case One, Two

enum ModeCoronagraph derives Order:
  case NoCoronagraph, Coronagraph

enum ModeSkysub derives Order:
  case Normal, High

enum ModeMOS derives Order:
  case NoMOS, MOS

case class ModeRow(
  instrument:           Instrument,
  mode:                 ObservationMode,
  fov:                  ModeFov,
  iqMin:                ModeIQ,
  iqMax:                ModeIQ,
  resolution:           PosBigDecimal,
  wavelength:           ModeWavelength,
  bandWidth:            ModeBandwidth,
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
    arcsecDecoder.map(x => ModeIQ(x))

  given CellDecoder[ModeFov] =
    arcsecDecoder.map(x => ModeFov(x))

  given CellDecoder[ModeBandwidth] =
    micrometerWavelengthDecoder.map(w => ModeBandwidth(w.µm.toValue[BigDecimal]))

  given CellDecoder[ModeGratingMinWavelength] =
    micrometerWavelengthDecoder.map(x => ModeGratingMinWavelength(x))

  given CellDecoder[ModeGratingMaxWavelength] =
    micrometerWavelengthDecoder.map(x => ModeGratingMaxWavelength(x))

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
      b    <- row.as[ModeBandwidth]("band_width")
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

case class ModesMatrix(matrix: List[ModeRow]) {

  val DefaultMinExp: Quantity[PosBigDecimal, Second] =
    BigDecimal(1).withRefinedUnit[Positive, Second]

  def spectroscopyModes(
    dwmin:       Option[ModeBandwidth],
    dwmax:       Option[ModeBandwidth],
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
    val l_dwmin                      = dwmin.getOrElse(ModeBandwidth.Zero)
    val l_dwmax                      = dwmax.getOrElse(ModeBandwidth.Ten)
    val criteria: ModeRow => Boolean = m =>
      m.mode === ObservationMode.Spectroscopy &&
        rmin.forall(m.resolution >= _) &&
        dims.forall(m.spatialDimensions >= _) &&
        m.bandWidth >= l_dwmin &&
        m.bandWidth <= l_dwmax &&
        coronograph.forall(_ === m.coronograph) &&
        m.minExposure <= mexp.getOrElse(DefaultMinExp) &&
        mos.forall(_ === m.mos) &&
        skysub.forall(_ === m.skySub) &&
        m.iqMin <= iqmax.getOrElse(defaultIQMax) &&
        m.fov >= fov.getOrElse(defaultFOV) &&
        wlen.forall(m.gratingMinWavelength.value <= _) &&
        wlen.forall(m.gratingMaxWavelength.value >= _)

    matrix.filter(criteria)
  }
}

object ModesMatrix extends ModesMatrixPlatform {
  val empty: ModesMatrix = ModesMatrix(Nil)
}
