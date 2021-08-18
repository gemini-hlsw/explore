// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect._
import cats.implicits._
import eu.timepit.refined._
import eu.timepit.refined.collection.NonEmpty
import explore.model.SSOConfig
import explore.model.UserVault
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.parser._
import lucuma.core.model.User
import lucuma.sso.client.codec.user._
import org.scalajs.dom.experimental.RequestCredentials
import org.scalajs.dom.window
import org.typelevel.log4cats.Logger
import retry._

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.{ util => ju }
import scala.concurrent.duration._
import org.http4s._
import org.http4s.dom.FetchClient

import ju.concurrent.TimeUnit
import _root_.cats.Applicative

final case class JwtOrcidProfile(exp: Long, `lucuma-user`: User)

object JwtOrcidProfile {
  implicit val decoder: Decoder[JwtOrcidProfile] = deriveDecoder
}

case class SSOClient[F[_]: Async: Logger](config: SSOConfig) {
  import RetryHelpers._

  // FIXME We need to add this functionality to http4s client, so that it sends cookies.
  // private val backend = FetchCatsBackend(FetchOptions(RequestCredentials.include.some, none))

  // Does a client side redirect to the sso site
  val redirectToLogin: F[Unit] =
    Sync[F].delay {
      val returnUrl = window.location
      window.location.href =
        (config.uri / "auth" / "v1" / "stage1").withQueryParam("state", returnUrl.toString).toString
    }

  val guest: F[UserVault] =
    retryingOnAllErrors(retryPolicy, logError("Switching to guest")) {
      FetchClient[F](credentials = RequestCredentials.include)
        .expect[String](Request[F](Method.POST, config.uri / "api" / "v1" / "auth-as-guest"))
        .map(body =>
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
        )
    }

  val whoami: F[Option[UserVault]] =
    retryingOnAllErrors(retryPolicy, logError("Calling whoami")) {
      FetchClient[F](credentials = RequestCredentials.include)
        .run(Request[F](Method.POST, config.uri / "api" / "v1" / "refresh-token"))
        .use {
          case Status.Successful(r) =>
            r.attemptAs[String]
              .leftMap(_.message)
              .value
              .map(
                _.flatMap(body =>
                  (for {
                    k <- Either
                           .catchNonFatal(
                             ju.Base64.getDecoder.decode(body.split('.')(1).replace("-", "+"))
                           )
                           .leftMap(_.getMessage)
                    j  = new String(k)
                    p <- parse(j).leftMap(_.message)
                    u <- p.as[JwtOrcidProfile].leftMap(_.message)
                    t <- refineV[NonEmpty](body)
                  } yield UserVault(u.`lucuma-user`, Instant.ofEpochSecond(u.exp), t))
                ).fold(msg => throw new RuntimeException(s"Error decoding the token: $msg"), _.some)
              )
          case r                    =>
            println(r)
            Applicative[F].pure(none[UserVault])
        }
    }
      .adaptError { case t =>
        new Exception("Error connecting to authentication server.", t)
      }

  def refreshToken(expiration: Instant): F[Option[UserVault]] =
    Sync[F].delay(Instant.now).flatMap { n =>
      val sleepTime = config.refreshTimeoutDelta.max(
        (n.until(expiration, ChronoUnit.SECONDS).seconds - config.refreshTimeoutDelta)
      )
      Temporal[F].sleep(sleepTime / config.refreshIntervalFactor)
    } >> whoami.flatTap(_ => Logger[F].info("User token refreshed"))

  val logout: F[Unit] =
    retryingOnAllErrors(retryPolicy, logError("Calling logout")) {
      FetchClient[F](credentials = RequestCredentials.include)
        .run(Request(Method.POST, config.uri / "api" / "v1" / "logout"))
        .use_
    }

  val switchToORCID: F[Unit] =
    logout.attempt >> redirectToLogin
}
