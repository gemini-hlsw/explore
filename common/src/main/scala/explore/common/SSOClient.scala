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
import sttp.model.Uri

final case class JwtOrcidProfile(exp: Long, `lucuma-user`: User)

object JwtOrcidProfile {
  implicit val decoder: Decoder[JwtOrcidProfile] = deriveDecoder
}

object SSOClient {
  type FromFuture[F[_], A] = F[Future[A]] => F[A]

  val ReadTimeout: FiniteDuration = 2.seconds

  // time before expiration to renew
  val RefreshTimoutDelta: FiniteDuration = 10.seconds

  // Does a client side redirect to the sso site
  def redirectToLogin[F[_]: Sync](ssoURI: Uri): F[Unit] =
    Sync[F].delay {
      val returnUrl = window.location
      println(ssoURI)
      window.location.href = uri"$ssoURI/auth/v1/stage1?state=$returnUrl".toString
    }

  def vault[F[_]: ConcurrentEffect: Logger](
    ssoURI:     Uri,
    fromFuture: FromFuture[F, Response[Either[String, String]]]
  ): F[UserVault] =
    for {
      d <- Deferred[F, UserVault]
      _ <- whoami[F](ssoURI, fromFuture).attempt.flatMap {
             case Right(Some(u)) => d.complete(u)
             case Right(None)    =>
               Logger[F].debug("Unauthenticated go to login form") *> guestUI[F](ssoURI,
                                                                                 d,
                                                                                 fromFuture
               )
             case Left(e)        =>
               Logger[F].error(e)("Error attempting to login") *> guestUI[F](ssoURI, d, fromFuture)
           }
      v <- d.get
    } yield v

  def guestUI[F[_]: Effect](
    ssoURI:     Uri,
    result:     Deferred[F, UserVault],
    fromFuture: FromFuture[F, Response[Either[String, String]]]
  ): F[Unit] = UserSelectionForm.launch[F](ssoURI, result, fromFuture).void

  def guest[F[_]: Sync](
    ssoURI:     Uri,
    fromFuture: FromFuture[F, Response[Either[String, String]]]
  ): F[UserVault] = {
    val backend  = FetchBackend(FetchOptions(RequestCredentials.include.some, none))
    def httpCall =
      Sync[F].delay(
        basicRequest
          .post(
            uri"$ssoURI/api/v1/auth-as-guest"
          )
          .readTimeout(ReadTimeout)
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
            } yield UserVault(u.`lucuma-user`, ssoURI, Instant.ofEpochSecond(u.exp), t))
              .getOrElse(throw new RuntimeException("Error decoding the token"))
          }
        case Response(Left(e), _, _, _, _, _)     =>
          throw new RuntimeException(e)
      }
  }

  private def whoami[F[_]: Sync](
    ssoURI:     Uri,
    fromFuture: FromFuture[F, Response[Either[String, String]]]
  ): F[Option[UserVault]] = {
    val backend  = FetchBackend(FetchOptions(RequestCredentials.include.some, none))
    val httpCall =
      Sync[F].delay(
        basicRequest
          .post(
            uri"$ssoURI/api/v1/refresh-token"
          )
          .readTimeout(ReadTimeout)
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
            } yield UserVault(u.`lucuma-user`, ssoURI, Instant.ofEpochSecond(u.exp), t).some)
              .getOrElse(throw new RuntimeException("Error decoding the token"))
          }
        case Response(Left(_), _, _, _, _, _)     =>
          none[UserVault].pure[F]
      }
  }

  def refreshToken[F[_]: ConcurrentEffect: Timer: Logger](
    ssoURI:     Uri,
    expiration: Instant,
    mod:        UserVault => F[Unit],
    fromFuture: FromFuture[F, Response[Either[String, String]]]
  ): F[UserVault] =
    Sync[F].delay(Instant.now).flatMap { n =>
      val sleepTime = RefreshTimoutDelta.max(
        (n.until(expiration, ChronoUnit.SECONDS).seconds - RefreshTimoutDelta)
      )
      Timer[F].sleep(sleepTime / 10)
    } *> Logger[F].info("refresh") *> SSOClient
      .vault[F](ssoURI, fromFuture)
      .flatTap(mod)

  def logout[F[_]: ConcurrentEffect: Logger](
    ssoURI:     Uri,
    fromFuture: FromFuture[F, Response[Either[String, String]]]
  ): F[Unit] = {
    val backend  = FetchBackend(FetchOptions(RequestCredentials.include.some, none))
    val httpCall =
      Sync[F].delay(
        basicRequest
          .post(
            uri"$ssoURI/api/v1/logout"
          )
          .readTimeout(ReadTimeout)
          .send(backend)
      )

    fromFuture(httpCall) *>
      Sync[F].delay(
        window.location.reload()
      ) // Let's just reload rather than trying to reset the state
  }
}
