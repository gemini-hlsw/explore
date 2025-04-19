// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Show
import cats.derived.*
import cats.effect.Async
import cats.syntax.all.*
import io.circe.*
import lucuma.core.enums.ExecutionEnvironment
import lucuma.ui.sso.SSOConfig
import org.http4s.Uri
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.syntax.all.*

case class AppConfig(
  hostName:         String,
  environment:      ExecutionEnvironment,
  preferencesDBURI: Uri,
  odbURI:           Uri,
  odbRestURI:       Uri,
  itcURI:           Uri,
  sso:              SSOConfig,
  tracing:          Option[TracingConfig]
) derives Eq,
      Show,
      Decoder

object AppConfig {
  private val configFile = uri"/environments.conf.json"

  private def fetchHelper[F[_]: Async](
    client:   Client[F],
    selector: List[AppConfig] => F[AppConfig]
  ): F[AppConfig] =
    client
      .get(configFile)(_.decodeJson[List[AppConfig]])
      .adaptError { case t =>
        new Exception("Could not retrieve configuration.", t)
      }
      .flatMap(selector)

  def fetchConfig[F[_]: Async](host: String, client: Client[F]): F[AppConfig] =
    fetchHelper(
      client,
      confs =>
        confs
          .find(conf => host.startsWith(conf.hostName))
          .orElse(confs.find(_.hostName === "*"))
          .fold(
            Async[F].raiseError(new Exception("Host not found in configuration."))
          )(_.pure)
    )

  def fetchConfig[F[_]: Async](env: ExecutionEnvironment, client: Client[F]): F[AppConfig] =
    fetchHelper(
      client,
      _.find(_.environment === env)
        .fold(
          Async[F].raiseError(new Exception("Environment not found in configuration."))
        )(_.pure)
    )
}
