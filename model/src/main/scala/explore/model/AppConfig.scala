// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
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

case class AppConfig(environment: ExecutionEnvironment, exploreDBURI: Uri, odbURI: Uri)

object AppConfig {
  implicit val eqAppConfig: Eq[AppConfig]     = Eq.fromUniversalEquals
  implicit val showAppConfig: Show[AppConfig] = Show.fromToString

  implicit val encoderAppConfig: Encoder[AppConfig] = deriveEncoder[AppConfig]
  implicit val decoderAppConfig: Decoder[AppConfig] = deriveDecoder[AppConfig]
}
