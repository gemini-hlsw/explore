// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.refTypeEq
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcAxis
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.SingleSN
import lucuma.itc.TotalSN
import lucuma.itc.client.SeriesResult
import lucuma.itc.client.TargetTimeAndGraphsResult
import lucuma.itc.math.roundToSignificantFigures

import scala.math.*

// Do not turn into enum or compositePickler will break.
sealed trait ItcQueryProblem(val message: String) derives Eq:
  def toTargetProblem: ItcTargetProblem = ItcTargetProblem(None, this)

object ItcQueryProblem:
  case object UnsupportedMode          extends ItcQueryProblem("Mode not supported")
  case object MissingWavelength        extends ItcQueryProblem("Wavelength is missing")
  case object MissingExposureTimeMode  extends ItcQueryProblem("Exposure time mode is missing")
  case object MissingTargetInfo        extends ItcQueryProblem("Target information is missing")
  case object MissingBrightness        extends ItcQueryProblem("Target brightness is missing")
  case class SourceTooBright(wellHalfFilledSeconds: BigDecimal)
      extends ItcQueryProblem(
        f"Source too bright, well half filled in $wellHalfFilledSeconds%.2f seconds"
      )
  case class GenericError(msg: String) extends ItcQueryProblem(msg)

case class ItcTargetProblem(targetName: Option[NonEmptyString], problem: ItcQueryProblem)
    derives Eq:
  def format: String =
    targetName.fold(problem.message)(name => s"$name: ${problem.message}")

// TODO: move to core
private given Eq[SignalToNoiseAt] = Eq.by(x => (x.wavelength, x.single, x.total))

sealed trait ItcResult derives Eq {
  def isSuccess: Boolean = this match {
    case ItcResult.Result(_, _, _, _) => true
    case _                            => false
  }

  def isPending: Boolean = this match {
    case ItcResult.Pending => true
    case _                 => false
  }
}

object ItcResult {
  case object Pending extends ItcResult
  case class Result(
    exposureTime:   TimeSpan,
    exposures:      PosInt,
    brightestIndex: Option[Int],
    snAt:           Option[SignalToNoiseAt]
  ) extends ItcResult:
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

case class ItcExposureTime(
  time:  TimeSpan,
  count: PosInt
) derives Eq

case class ItcGraphResult(target: ItcTarget, timeAndGraphs: TargetTimeAndGraphsResult) derives Eq {
  export timeAndGraphs.*

  private lazy val time: IntegrationTime = timeAndGraphs.integrationTime.times.focus

  lazy val itcExposureTime: ItcExposureTime =
    ItcExposureTime(time.exposureTime, time.exposureCount)

  lazy val finalSNRatio: TotalSN =
    timeAndGraphs.atWavelengthFinalSNRatio.getOrElse(timeAndGraphs.peakFinalSNRatio)

  lazy val singleSNRatio: SingleSN =
    timeAndGraphs.atWavelengthSingleSNRatio.getOrElse(timeAndGraphs.peakSingleSNRatio)
}

case class ItcAsterismGraphResults(
  asterismGraphs:  Map[ItcTarget, Either[ItcQueryProblem, ItcGraphResult]],
  brightestTarget: Option[ItcTarget],
  signalToNoiseAt: Wavelength
) derives Eq:
  val targets: List[ItcTarget] = asterismGraphs.keySet.toList.sortBy(_.name.value)
