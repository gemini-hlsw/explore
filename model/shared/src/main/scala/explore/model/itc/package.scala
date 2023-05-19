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
import io.circe.Decoder
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.given
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.itc.ItcAxis
import lucuma.itc.ItcCcd
import lucuma.itc.ItcSeries
import lucuma.itc.client.OptimizedChartResult
import lucuma.itc.client.OptimizedSeriesResult
import lucuma.itc.math.roundToSignificantFigures
import lucuma.schemas.decoders.given

import scala.concurrent.duration.*
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
      NonNegInt
        .from(count.value)
        .toOption
        .map(c => ItcExposureTime(OverridenExposureTime.FromItc, time, c))
    case _                             => none
  }
}

object ItcResult {
  case object Pending                                          extends ItcResult
  case class Result(exposureTime: TimeSpan, exposures: PosInt) extends ItcResult:
    val duration: TimeSpan = exposureTime *| exposures.value
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

opaque type OverridenExposureTime = Boolean

object OverridenExposureTime:
  val Overriden: OverridenExposureTime = true
  val FromItc: OverridenExposureTime   = false

  given Eq[OverridenExposureTime] = Eq.catsKernelInstancesForBoolean

case class ItcExposureTime(
  overriden: OverridenExposureTime,
  time:      TimeSpan,
  count:     NonNegInt
) derives Eq

case class ItcChartResult(
  target:              ItcTarget,
  ccds:                NonEmptyList[ItcCcd],
  charts:              NonEmptyList[OptimizedChartResult],
  peakSNRatio:         SignalToNoise,
  atWavelengthSNRatio: Option[SignalToNoise]
)
