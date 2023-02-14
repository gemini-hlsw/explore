// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import io.circe.Decoder
import io.circe.generic.semiauto
import io.circe.refined.*
import lucuma.core.util.TimeSpan
import lucuma.schemas.decoders.given
import monocle.Focus
import monocle.Prism
import monocle.macros.GenPrism

sealed trait OdbItcResult extends Product with Serializable

// Currently the ODB and ITC have `ItcResult` types, and they don't match.
// At some point we may unify these, or always go directly to the ITC instead
// of getting ITC data via the ODB.
object OdbItcResult {
  case class Success(
    exposureTime:  TimeSpan,
    exposures:     NonNegInt,
    signalToNoise: PosBigDecimal
  ) extends OdbItcResult

  case class MissingParams(
    params: List[String]
  ) extends OdbItcResult

  case class ServiceError(
    message: String
  ) extends OdbItcResult

  val success: Prism[OdbItcResult, Success]             = GenPrism[OdbItcResult, Success]
  val missingParams: Prism[OdbItcResult, MissingParams] = GenPrism[OdbItcResult, MissingParams]
  val serviceError: Prism[OdbItcResult, ServiceError]   = GenPrism[OdbItcResult, ServiceError]

  given Decoder[Success]       = semiauto.deriveDecoder
  given Decoder[MissingParams] = semiauto.deriveDecoder
  given Decoder[ServiceError]  = semiauto.deriveDecoder

  given Decoder[OdbItcResult] = Decoder.instance { c =>
    c.downField("result")
      .as[Success]
      .orElse(c.downField("result").as[MissingParams])
      .orElse(c.downField("result").as[ServiceError])
  }
}
