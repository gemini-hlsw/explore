// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import java.{ util => ju }

import scala.concurrent.Future
import scala.concurrent.duration._

import cats.effect._
import cats.effect.concurrent.Deferred
import cats.implicits._
import eu.timepit.refined._
import eu.timepit.refined.collection.NonEmpty
import explore.model.UserVault
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
import explore.components.UserSelectionForm
import io.chrisdavenport.log4cats.Logger

final case class JwtOrcidProfile(exp: Long, `lucuma-user`: User)

object JwtOrcidProfile {
  implicit val decoder: Decoder[JwtOrcidProfile] = deriveDecoder
}

object SSOClient {
  type FromFuture[F[_], A] = F[Future[A]] => F[A]

  // time before expiration to renew
  val refreshTimoutDelta: FiniteDuration = 10.seconds

  val baseSSO = uri"https://sso.gpp.lucuma.xyz"

  // Does a client side redirect to the sso site
  def redirectToLogin[F[_]: Sync]: F[Unit] =
    Sync[F].delay {
      val returnUrl = window.location
      window.location.href = uri"$baseSSO/auth/v1/stage1?state=$returnUrl".toString
    }

  def vault[F[_]: ConcurrentEffect: Logger](
    fromFuture: FromFuture[F, Response[Either[String, String]]]
  ): F[UserVault] =
    for {
      d <- Deferred[F, UserVault]
      _ <- whoami[F](fromFuture).attempt.flatMap {
             case Right(Some(u)) => d.complete(u)
             case Right(None)    =>
               Logger[F].debug("Unauthenticated go to login form") *> guestUI[F](d, fromFuture)
             case Left(e)        =>
               Logger[F].error(e)("Error attempting to login") *> guestUI[F](d, fromFuture)
           }
      v <- d.get
    } yield v

  def guestUI[F[_]: Sync: Effect](
    result:     Deferred[F, UserVault],
    fromFuture: FromFuture[F, Response[Either[String, String]]]
  ): F[Unit] = UserSelectionForm.launch[F](result, fromFuture).void

  def guest[F[_]: Sync](
    fromFuture: FromFuture[F, Response[Either[String, String]]]
  ): F[UserVault] = {
    val backend  = FetchBackend(FetchOptions(RequestCredentials.include.some, none))
    def httpCall =
      Sync[F].delay(
        basicRequest
          .post(
            uri"$baseSSO/api/v1/auth-as-guest"
          )
          .readTimeout(5.seconds)
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

  private def whoami[F[_]: Sync](
    fromFuture: FromFuture[F, Response[Either[String, String]]]
  ): F[Option[UserVault]] = {
    val backend  = FetchBackend(FetchOptions(RequestCredentials.include.some, none))
    val httpCall =
      Sync[F].delay(
        basicRequest
          .post(
            uri"$baseSSO/api/v1/refresh-token"
          )
          .readTimeout(5.seconds)
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

  def refreshToken[F[_]: ConcurrentEffect: Timer: Logger](
    fromFuture: FromFuture[F, Response[Either[String, String]]],
    expiration: Instant,
    mod:        UserVault => F[Unit]
  ): F[UserVault] =
    Sync[F].delay(Instant.now).flatMap { n =>
      val sleepTime = refreshTimoutDelta.max(
        (n.until(expiration, ChronoUnit.SECONDS).seconds - refreshTimoutDelta)
      )
      Timer[F].sleep(sleepTime / 10)
    } *> SSOClient
      .vault[F](fromFuture)
      .flatTap(mod)

}
