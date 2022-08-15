// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.Eq
import cats.derived.*
import cats.syntax.all._
import eu.timepit.refined.auto._
import explore.model.enums.ItcSeriesDataType
import io.circe.Decoder
import lucuma.core.util.Enumerated

import scala.concurrent.duration._
import eu.timepit.refined.types.numeric.PosInt

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
  case class Result(exposureTime: FiniteDuration, exposures: Int) extends ItcResult {
    val duration: FiniteDuration = exposureTime * exposures.toLong
  }
}

final case class ItcChart(title: String, dataType: ItcSeriesDataType, data: List[(Double, Double)])

object remote:
  import scala.math._

  def roundToSignificantFigures(num: Double, n: Int): Double =
    if num == 0 then 0
    else
      val d     = ceil(log10(abs(num)))
      val power = n - d.toInt

      val magnitude = pow(10, power)
      val shifted   = round(num * magnitude)
      shifted / magnitude

  final case class Axis(start: Double, end: Double, count: Int) derives Decoder:
    val step = (end - start) / (count - 1)

  final case class ItcChartRemote(
    title:    String,
    dataType: ItcSeriesDataType,
    xAxis:    Axis,
    dataY:    List[Double]
  ) derives Decoder:
    def toItcChart: ItcChart =
      val genData = dataY.zipWithIndex.map((y, i) =>
        (roundToSignificantFigures(xAxis.step * i + xAxis.start, 4), y)
      )
      ItcChart(title, dataType, genData)
