// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.refTypeEq
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.NewType
import lucuma.core.util.TimeSpan
import lucuma.itc.FinalSN
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcAxis
import lucuma.itc.SingleSN
import lucuma.itc.client.SeriesResult
import lucuma.itc.client.TargetTimeAndGraphsResult
import lucuma.itc.math.roundToSignificantFigures

import scala.math.*

// Do not turn into enum or compositePickler will break.
sealed trait ItcQueryProblem(val message: String) derives Eq
object ItcQueryProblem:
  case object UnsupportedMode          extends ItcQueryProblem("Unsupported mode")
  case object MissingWavelength        extends ItcQueryProblem("Missing wavelength")
  case object MissingSignalToNoise     extends ItcQueryProblem("Missing signal to noise")
  case object MissingSignalToNoiseAt
      extends ItcQueryProblem("Missing signal to noise at wavelength")
  case object MissingTargetInfo        extends ItcQueryProblem("Missing target info")
  case object MissingBrightness        extends ItcQueryProblem("Missing brightness")
  case class SourceTooBright(wellHalfFilledSeconds: BigDecimal)
      extends ItcQueryProblem(s"Source too bright")
  case class GenericError(msg: String) extends ItcQueryProblem(msg)

case class ItcTargetProblem(targetName: Option[NonEmptyString], problem: ItcQueryProblem) derives Eq

sealed trait ItcResult extends Product with Serializable derives Eq {
  def isSuccess: Boolean = this match {
    case ItcResult.Result(_, _, _) => true
    case _                         => false
  }

  def isPending: Boolean = this match {
    case ItcResult.Pending => true
    case _                 => false
  }
}

object ItcResult {
  case object Pending extends ItcResult
  case class Result(exposureTime: TimeSpan, exposures: NonNegInt, brightestIndex: Option[Int])
      extends ItcResult:
    val duration: TimeSpan        = exposureTime *| exposures.value
    override def toString: String = s"${exposures.value} x ${exposureTime.toMinutes}"
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

extension (a: SeriesResult)
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
  count:     NonNegInt
) derives Eq

case class ItcGraphResult(target: ItcTarget, timeAndGraphs: TargetTimeAndGraphsResult) {
  export timeAndGraphs.*

  private lazy val time: IntegrationTime = timeAndGraphs.integrationTime.times.focus

  lazy val itcExposureTime: ItcExposureTime =
    ItcExposureTime(OverridenExposureTime.FromItc, time.exposureTime, time.exposureCount)

  lazy val finalSNRatio: FinalSN =
    timeAndGraphs.atWavelengthFinalSNRatio.getOrElse(timeAndGraphs.peakFinalSNRatio)

  lazy val singleSNRatio: SingleSN =
    timeAndGraphs.atWavelengthSingleSNRatio.getOrElse(timeAndGraphs.peakSingleSNRatio)
}

case class ItcAsterismGraphResults(
  asterismGraphs:  Map[ItcTarget, Either[ItcQueryProblem, ItcGraphResult]],
  brightestTarget: Option[ItcTarget]
)
