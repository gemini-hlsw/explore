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
      Decoder {
  val readTimeout: FiniteDuration         = FiniteDuration(readTimeoutSeconds, TimeUnit.SECONDS)
  val refreshTimeoutDelta: FiniteDuration =
    FiniteDuration(refreshTimeoutDeltaSeconds, TimeUnit.SECONDS)
}

case class AppConfig(
  hostName:         String,
  environment:      ExecutionEnvironment,
  preferencesDBURI: Uri,
  odbURI:           Uri,
  odbRestURI:       Uri,
  itcURI:           Uri,
  sso:              SSOConfig
) derives Eq,
      Show,
      Decoder {}

object AppConfig {
  private val configFile = uri"/environments.conf.json"

  def fetchConfig[F[_]: Async](host: String, client: Client[F]): F[AppConfig] =
    client
      .get(configFile)(_.decodeJson[List[AppConfig]])
      .adaptError { case t =>
        new Exception("Could not retrieve configuration.", t)
      }
      .flatMap(confs =>
        confs
          .find(conf => host.startsWith(conf.hostName))
          .orElse(confs.find(_.hostName === "*"))
          .fold(
            Async[F].raiseError(new Exception("Host not found in configuration."))
          )(_.pure)
      )

  def fetchConfig[F[_]: Async](env: ExecutionEnvironment, client: Client[F]): F[AppConfig] =
    client
      .get(configFile)(_.decodeJson[List[AppConfig]])
      .adaptError { case t =>
        new Exception("Could not retrieve configuration.", t)
      }
      .flatMap(
        _.find(_.environment === env)
          .fold(
            Async[F].raiseError(new Exception("Environment not found in configuration."))
          )(_.pure)
      )
}
