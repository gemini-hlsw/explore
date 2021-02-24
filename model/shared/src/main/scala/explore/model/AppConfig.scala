// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Show
import explore.model.decoders._
import explore.model.encoders._
import explore.model.enum.ExecutionEnvironment
import io.circe._
import io.circe.generic.semiauto._
import sttp.model.Uri

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

case class SSOConfig(
  uri:                        Uri,
  readTimeoutSeconds:         Long = 10,
  refreshTimeoutDeltaSeconds: Long = 10, // time before expiration to renew
  refreshIntervalFactor:      Long = 1
) {
  val readTimeout: FiniteDuration         = FiniteDuration(readTimeoutSeconds, TimeUnit.SECONDS)
  val refreshTimeoutDelta: FiniteDuration =
    FiniteDuration(refreshTimeoutDeltaSeconds, TimeUnit.SECONDS)
}

object SSOConfig {
  implicit val eqSSOConfig: Eq[SSOConfig]     = Eq.fromUniversalEquals
  implicit val showSSOConfig: Show[SSOConfig] = Show.fromToString

  implicit val encoderSSOConfig: Encoder[SSOConfig] = deriveEncoder[SSOConfig]
  implicit val decoderSSOConfig: Decoder[SSOConfig] = deriveDecoder[SSOConfig]
}

case class AppConfig(
  environment:      ExecutionEnvironment,
  preferencesDBURI: Uri,
  odbURI:           Uri,
  sso:              SSOConfig
)

object AppConfig {
  implicit val eqAppConfig: Eq[AppConfig]     = Eq.fromUniversalEquals
  implicit val showAppConfig: Show[AppConfig] = Show.fromToString

  implicit val encoderAppConfig: Encoder[AppConfig] = deriveEncoder[AppConfig]
  implicit val decoderAppConfig: Decoder[AppConfig] = deriveDecoder[AppConfig]
}
