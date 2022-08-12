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

final case class ItcChart(title: String, dataType: ItcSeriesDataType, data: List[List[Double]])
    derives Decoder
