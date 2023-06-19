// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Show
import cats.derived.*
import cats.effect.Async
import cats.syntax.all.*
import explore.model.enums.ExecutionEnvironment
import io.circe.*
import io.circe.generic.semiauto.*
import org.http4s.Uri
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.syntax.all.*

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

case class SSOConfig(
  uri:                        Uri,
  readTimeoutSeconds:         Long = 3,
  refreshTimeoutDeltaSeconds: Long = 10, // time before expiration to renew
  refreshIntervalFactor:      Long = 1
) derives Eq,
      Show,
      Encoder.AsObject,
      Decoder {
  val readTimeout: FiniteDuration         = FiniteDuration(readTimeoutSeconds, TimeUnit.SECONDS)
  val refreshTimeoutDelta: FiniteDuration =
    FiniteDuration(refreshTimeoutDeltaSeconds, TimeUnit.SECONDS)
}

case class AppConfig(
  environment:      ExecutionEnvironment,
  preferencesDBURI: Uri,
  odbURI:           Uri,
  odbRestURI:       Uri,
  itcURI:           Uri,
  sso:              SSOConfig
) derives Eq,
      Show,
      Encoder.AsObject,
      Decoder {}

object AppConfig {

  def confEnvironment(host: String): ExecutionEnvironment = host match {
    case host if host.startsWith("explore.gemini.edu") => ExecutionEnvironment.Production
    case host if host.startsWith("explore.lucuma.xyz") => ExecutionEnvironment.Staging
    case _                                             => ExecutionEnvironment.Development
  }

  def confName(env: ExecutionEnvironment) = env match {
    case ExecutionEnvironment.Development => uri"/development.conf.json"
    case ExecutionEnvironment.Staging     => uri"/staging.conf.json"
    case ExecutionEnvironment.Production  => uri"/production.conf.json"
  }

  def fetchConfig[F[_]: Async](host: String, client: Client[F]): F[AppConfig] =
    client
      .get(confName(confEnvironment(host)))(_.decodeJson[AppConfig])
      .adaptError { case t =>
        new Exception("Could not retrieve configuration.", t)
      }

  def fetchConfig[F[_]: Async](env: ExecutionEnvironment, client: Client[F]): F[AppConfig] =
    client
      .get(confName(env))(_.decodeJson[AppConfig])
      .adaptError { case t =>
        new Exception("Could not retrieve configuration.", t)
      }
}
