// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect._
import cats.implicits._
import eu.timepit.refined._
import eu.timepit.refined.collection.NonEmpty
import explore.model.SSOConfig
import explore.model.UserVault
import io.chrisdavenport.log4cats.Logger
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.parser._
import lucuma.core.model.User
import lucuma.sso.client.codec.user._
import org.scalajs.dom.experimental.RequestCredentials
import org.scalajs.dom.window
import sttp.client3._

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.{ util => ju }
import scala.concurrent.Future
import scala.concurrent.duration._

final case class JwtOrcidProfile(exp: Long, `lucuma-user`: User)

object JwtOrcidProfile {
  implicit val decoder: Decoder[JwtOrcidProfile] = deriveDecoder
}

object SSOClient {
  type FromFuture[F[_], A] = F[Future[A]] => F[A]
}

case class SSOClient[F[_]: ConcurrentEffect: Timer: Logger](
  config:     SSOConfig,
  fromFuture: SSOClient.FromFuture[F, Response[Either[String, String]]]
) {
  // Does a client side redirect to the sso site
  val redirectToLogin: F[Unit] =
    Sync[F].delay {
      val returnUrl = window.location
      window.location.href = uri"${config.uri}/auth/v1/stage1?state=$returnUrl".toString
    }

  val guest: F[UserVault] = {
    val backend  = FetchBackend(FetchOptions(RequestCredentials.include.some, none))
    def httpCall =
      Sync[F].delay(
        basicRequest
          .post(
            uri"${config.uri}/api/v1/auth-as-guest"
          )
          .readTimeout(config.readTimeout)
          .send(backend)
      )

    fromFuture(httpCall)
      .flatMap {
        case Response(Right(body), _, _, _, _, _) =>
          Sync[F].delay {
            (for {
              k <- Either.catchNonFatal(
                     ju.Base64.getDecoder.decode(body.split('.')(1).replace("-", "+"))
                   )
              j  = new String(k)
              p <- parse(j)
              u <- p.as[JwtOrcidProfile]
              t <- refineV[NonEmpty](body)
            } yield UserVault(u.`lucuma-user`, Instant.ofEpochSecond(u.exp), t))
              .getOrElse(throw new RuntimeException("Error decoding the token"))
          }
        case Response(Left(e), _, _, _, _, _)     =>
          throw new RuntimeException(e)
      }
  }

  val whoami: F[Option[UserVault]] = {
    val backend  = FetchBackend(FetchOptions(RequestCredentials.include.some, none))
    val httpCall =
      Sync[F].delay(
        basicRequest
          .post(
            uri"${config.uri}/api/v1/refresh-token"
          )
          .readTimeout(config.readTimeout)
          .send(backend)
      )

    fromFuture(httpCall)
      .flatMap {
        case Response(Right(body), _, _, _, _, _) =>
          Sync[F].delay {
            (for {
              k <- Either.catchNonFatal(
                     ju.Base64.getDecoder.decode(body.split('.')(1).replace("-", "+"))
                   )
              j  = new String(k)
              p <- parse(j)
              u <- p.as[JwtOrcidProfile]
              t <- refineV[NonEmpty](body)
            } yield UserVault(u.`lucuma-user`, Instant.ofEpochSecond(u.exp), t).some)
              .getOrElse(throw new RuntimeException("Error decoding the token"))
          }
        case Response(Left(_), _, _, _, _, _)     =>
          none[UserVault].pure[F]
      }
  }

  def refreshToken(expiration: Instant): F[Option[UserVault]] =
    Sync[F].delay(Instant.now).flatMap { n =>
      val sleepTime = config.refreshTimeoutDelta.max(
        (n.until(expiration, ChronoUnit.SECONDS).seconds - config.refreshTimeoutDelta)
      )
      Timer[F].sleep(sleepTime / config.refreshIntervalFactor)
    } >> whoami.flatTap(_ => Logger[F].info("User token refreshed"))

  val logout: F[Unit] = {
    val backend  = FetchBackend(FetchOptions(RequestCredentials.include.some, none))
    val httpCall =
      Sync[F].delay(
        basicRequest
          .post(
            uri"${config.uri}/api/v1/logout"
          )
          .readTimeout(config.readTimeout)
          .send(backend)
      )

    fromFuture(httpCall).void
  }

  val switchToORCID: F[Unit] =
    logout.attempt >> redirectToLogin
}
