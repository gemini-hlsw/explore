// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.Eq
import cats.derived.*
import cats.syntax.all._
import eu.timepit.refined.auto._

import scala.concurrent.duration._

enum ItcQueryProblems derives Eq {
  case UnsupportedMode
  case MissingWavelength
  case MissingSignalToNoise
  case MissingTargetInfo
  case GenericError(msg: String)
}

sealed trait ItcResult extends Product with Serializable derives Eq

object ItcResult {
  case object SourceTooBright                                     extends ItcResult
  case object Pending                                             extends ItcResult
  case class Result(exposureTime: FiniteDuration, exposures: Int) extends ItcResult {
    val duration: FiniteDuration = exposureTime * exposures.toLong
  }
}
