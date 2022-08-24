// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all._
import eu.timepit.refined.cats.refTypeEq
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import explore.model.enums.ItcSeriesType
import explore.model.enums.ItcChartType
import io.circe.Decoder
import lucuma.core.model.NonNegDuration
import lucuma.core.model.implicits.*
import lucuma.core.util.Enumerated
import lucuma.utils.NewType

import scala.concurrent.duration._
import scala.math._

sealed trait ItcQueryProblems extends Product with Serializable derives Eq

object ItcQueryProblems {
  case object UnsupportedMode          extends ItcQueryProblems
  case object MissingWavelength        extends ItcQueryProblems
  case object MissingSignalToNoise     extends ItcQueryProblems
  case object MissingTargetInfo        extends ItcQueryProblems
  case class GenericError(msg: String) extends ItcQueryProblems
}

sealed trait ItcResult extends Product with Serializable derives Eq

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
  time:      NonNegDuration,
  count:     NonNegInt
) derives Eq

case class ItcCcd(
  singleSNRatio: Double, // the final SN ratio for a single image
  totalSNRatio:  Double, // the total SN ratio for all images
  peakPixelFlux: Double, // the highest e- count for all pixels on the CCD
  ampGain:       Double  // the amplifier gain for this CCD (used to calculate ADU)
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

case class ItcChartResult(ccds: NonEmptyList[ItcCcd], charts: NonEmptyList[ItcChart])

object math:
  extension (ccds: NonEmptyList[ItcCcd])
    def maxPeakPixelFlux: Int    = ccds.maximumBy(_.peakPixelFlux).peakPixelFlux.toInt
    def maxSingleSNRatio: Double = ccds.maximumBy(_.singleSNRatio).singleSNRatio
    def maxTotalSNRatio: Double  = ccds.maximumBy(_.totalSNRatio).totalSNRatio
    def maxADU: Int              = ccds.maximumBy(_.adu).adu

  def roundToSignificantFigures(num: Double, n: Int): Double =
    if num == 0 then 0
    else
      val d     = ceil(log10(abs(num)))
      val power = n - d.toInt

      val magnitude = pow(10, power)
      val shifted   = round(num * magnitude)
      shifted / magnitude

  /**
   * Returns a "nice" number approximately equal to range Rounds the number if round = true Takes
   * the ceiling if round = false.
   */
  def niceNum(range: Double, round: Boolean): Double =
    val exponent = floor(log10(range))
    val fraction = range / pow(10, exponent)

    val niceFraction =
      if round then
        fraction match
          case f if f < 1.5 => 1
          case f if f < 3   => 2
          case f if f < 7   => 5
          case _            => 10
      else
        fraction match
          case f if f <= 1 => 1
          case f if f <= 2 => 2
          case f if f <= 5 => 5
          case _           => 10

    niceFraction * pow(10, exponent)

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
        (math.roundToSignificantFigures(xAxis.step * i + xAxis.start, 4), y)
      )
      ItcSeries(title, seriesType, genData, yAxis)
