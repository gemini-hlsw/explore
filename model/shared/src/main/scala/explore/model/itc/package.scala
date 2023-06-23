// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.refTypeEq
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.util.NewType
import lucuma.core.util.TimeSpan
import lucuma.itc.FinalSN
import lucuma.itc.ItcAxis
import lucuma.itc.ItcCcd
import lucuma.itc.SingleSN
import lucuma.itc.client.OptimizedChartResult
import lucuma.itc.client.OptimizedSeriesResult
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphResult
import lucuma.itc.math.roundToSignificantFigures

import scala.math.*

sealed trait ItcQueryProblems extends Product with Serializable derives Eq

object ItcQueryProblems {
  case object UnsupportedMode          extends ItcQueryProblems
  case object MissingWavelength        extends ItcQueryProblems
  case object MissingSignalToNoise     extends ItcQueryProblems
  case object MissingTargetInfo        extends ItcQueryProblems
  case object MissingBrightness        extends ItcQueryProblems
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

  def toItcExposureTime: Option[ItcExposureTime] = this match {
    case ItcResult.Result(time, count) =>
      ItcExposureTime(OverridenExposureTime.FromItc, time, count).some
    case _                             => none
  }
}

object ItcResult {
  case object Pending                                          extends ItcResult
  case class Result(exposureTime: TimeSpan, exposures: PosInt) extends ItcResult:
    val duration: TimeSpan        = exposureTime *| exposures.value
    override def toString: String = s"${exposures.value} x ${exposureTime.toMinutes}"

  def fromItcExposureTime(e: ItcExposureTime): ItcResult =
    Result(e.time, e.count)
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

extension (a: Option[ItcAxis])
  def yAxis: YAxis = a.map(a => YAxis(a.min, a.max)).getOrElse(YAxis.Empty)

extension (a: ItcAxis) def step = (a.end - a.start) / (a.count - 1)

extension (a: OptimizedSeriesResult)
  def data =
    val step  = a.xAxis.map(_.step).getOrElse(1.0)
    val start = a.xAxis.map(_.start).getOrElse(1.0)
    a.dataY.zipWithIndex.map((y, i) => (roundToSignificantFigures(step * i + start, 6), y))

object OverridenExposureTime extends NewType[Boolean]:
  val Overriden: OverridenExposureTime = OverridenExposureTime(true)
  val FromItc: OverridenExposureTime   = OverridenExposureTime(false)

type OverridenExposureTime = OverridenExposureTime.Type

case class ItcExposureTime(
  overriden: OverridenExposureTime,
  time:      TimeSpan,
  count:     PosInt
) derives Eq

case class ItcChartResult(
  target:                    ItcTarget,
  itcExposureTime:           ItcExposureTime,
  ccds:                      NonEmptyList[ItcCcd],
  charts:                    NonEmptyList[OptimizedChartResult],
  peakFinalSNRatio:          FinalSN,
  atWavelengthFinalSNRatio:  Option[FinalSN],
  peakSingleSNRatio:         SingleSN,
  atWavelengthSingleSNRatio: Option[SingleSN]
) {
  val finalSNRatio  = atWavelengthFinalSNRatio.getOrElse(peakFinalSNRatio)
  val singleSNRatio = atWavelengthSingleSNRatio.getOrElse(peakSingleSNRatio)
}

extension (a: SpectroscopyIntegrationTimeAndGraphResult)
  def toItcExposureTime: ItcExposureTime =
    ItcExposureTime(OverridenExposureTime.FromItc, a.exposureTime, a.exposures)
