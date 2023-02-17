// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.refTypeEq
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import explore.model.enums.ItcChartType
import explore.model.enums.ItcSeriesType
import io.circe.Decoder
import lucuma.core.math.Wavelength
import lucuma.core.model.given
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.schemas.decoders.given

import scala.concurrent.duration.*
import scala.math.*

sealed trait ItcQueryProblems extends Product with Serializable derives Eq

object ItcQueryProblems {
  case object UnsupportedMode          extends ItcQueryProblems
  case object MissingWavelength        extends ItcQueryProblems
  case object MissingSignalToNoise     extends ItcQueryProblems
  case object MissingTargetInfo        extends ItcQueryProblems
  case class GenericError(msg: String) extends ItcQueryProblems
}

sealed trait ItcResult extends Product with Serializable derives Eq {
  def isSuccess: Boolean = this match {
    case ItcResult.Result(_, _) => true
    case _                      => false
  }

  def isPending: Boolean = this match {
    case ItcResult.Pending => true
    case _                 => false
  }

  def toChartExposureTime: Option[ItcChartExposureTime] = this match {
    case ItcResult.Result(time, count) =>
      (TimeSpan.fromMicroseconds(time.toMicros), NonNegInt.from(count).toOption).mapN((t, c) =>
        ItcChartExposureTime(OverridenExposureTime.FromItc, t, c)
      )
    case _                             => none
  }
}

object ItcResult {
  case object SourceTooBright                                     extends ItcResult
  case object Pending                                             extends ItcResult
  case class Result(exposureTime: FiniteDuration, exposures: Int) extends ItcResult:
    val duration: FiniteDuration = exposureTime * exposures.toLong
}

case class YAxis(min: Double, max: Double):
  def ticks(maxTicks: Int = 10) =
    val range       = math.niceNum(max - min, false)
    val tickSpacing = math.niceNum(range / (maxTicks - 1), true)
    val niceMin     =
      floor(min / tickSpacing) * tickSpacing
    val niceMax     =
      ceil(max / tickSpacing) * tickSpacing

    (niceMin, niceMax, tickSpacing)

  def ∪(that: YAxis): YAxis = YAxis(min.min(that.min), max.max(that.max))

end YAxis

object YAxis:
  val Empty: YAxis = YAxis(0, 0)

opaque type OverridenExposureTime = Boolean

object OverridenExposureTime:
  val Overriden: OverridenExposureTime = true
  val FromItc: OverridenExposureTime   = false

  given Eq[OverridenExposureTime] = Eq.catsKernelInstancesForBoolean

case class ItcChartExposureTime(
  overriden: OverridenExposureTime,
  time:      TimeSpan,
  count:     NonNegInt
) derives Eq

case class ItcCcd(
  singleSNRatio:                Double, // the final SN ratio for a single image
  totalSNRatio:                 Double, // the total SN ratio for all images
  maxTotalSNRatio:              Double, // the total SN ratio for all images
  peakPixelFlux:                Double, // the highest e- count for all pixels on the CCD
  ampGain:                      Double, // the amplifier gain for this CCD (used to calculate ADU)
  wavelengthForMaxTotalSNRatio: Wavelength
) derives Decoder {
  val adu: Int = (peakPixelFlux / ampGain).toInt // the ADU value
}

case class ItcSeries(
  title:      String,
  seriesType: ItcSeriesType,
  data:       List[(Double, Double)],
  yAxis:      YAxis
)

case class ItcChart(
  chartType: ItcChartType,
  series:    List[ItcSeries]
)

case class ItcChartResult(
  target: ItcTarget,
  ccds:   NonEmptyList[ItcCcd],
  charts: NonEmptyList[ItcChart]
)

object remote:
  case class XAxis(start: Double, end: Double, count: Int) derives Decoder:
    val step = (end - start) / (count - 1)

  case class ItcChartGroupRemote(chartType: ItcChartType, series: List[ItcChartRemote])
      derives Decoder:
    def toItcChart: ItcChart = ItcChart(chartType, series.map(_.toItcSeries))

  case class ItcChartRemote(
    title:      String,
    seriesType: ItcSeriesType,
    xAxis:      XAxis,
    yAxis:      YAxis,
    dataY:      List[Double]
  ) derives Decoder:
    def toItcSeries: ItcSeries =
      val genData = dataY.zipWithIndex.map((y, i) =>
        (math.roundToSignificantFigures(xAxis.step * i + xAxis.start, 6), y)
      )
      ItcSeries(title, seriesType, genData, yAxis)
