// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.auto._

import scala.concurrent.duration._

sealed trait ItcQueryProblems extends Product with Serializable

object ItcQueryProblems {
  case object UnsupportedMode          extends ItcQueryProblems
  case object MissingWavelength        extends ItcQueryProblems
  case object MissingSignalToNoise     extends ItcQueryProblems
  case object MissingTargetInfo        extends ItcQueryProblems
  case class GenericError(msg: String) extends ItcQueryProblems

  implicit val eq: Eq[ItcQueryProblems] = Eq.instance {
    case (UnsupportedMode, UnsupportedMode)           => true
    case (MissingWavelength, MissingWavelength)       => true
    case (MissingSignalToNoise, MissingSignalToNoise) => true
    case (MissingTargetInfo, MissingTargetInfo)       => true
    case (GenericError(a), GenericError(b))           => a === b
    case _                                            => false
  }
}

sealed trait ItcResult extends Product with Serializable

object ItcResult {
  case object SourceTooBright                                     extends ItcResult
  case object Pending                                             extends ItcResult
  case class Result(exposureTime: FiniteDuration, exposures: Int) extends ItcResult {
    val duration: FiniteDuration = exposureTime * exposures.toLong
  }

  implicit val eq: Eq[ItcResult] = Eq.instance {
    case (SourceTooBright, SourceTooBright) => true
    case (Pending, Pending)                 => true
    case (Result(t1, e1), Result(t2, e2))   => t1 === t2 && e1 === e2
    case _                                  => false
  }
}
