// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import boopickle.DefaultBasic.*
import cats.*
import cats.effect.*
import cats.syntax.all.*
import explore.model.boopickle.ItcPicklers.given
import lucuma.itc.ItcVersions
import lucuma.itc.client.ItcClient
import org.typelevel.log4cats.Logger
import workers.*

object ITCVersionsRequests {

  def queryItc[F[_]: Concurrent: Parallel: Logger](
    cache:     Cache[F],
    itcClient: ItcClient[F]
  ): F[Unit] =

    val cacheName                                         = CacheName("itcVersions")
    val cacheVersion                                      = CacheVersion(1)
    val cacheableRequest: Cacheable[F, Unit, ItcVersions] =
      Cacheable(
        cacheName,
        cacheVersion,
        _ => itcClient.versions
      )

    for {
      _        <- Logger[F].debug(s"ITC: Request version")
      existing <- cache.get[Unit, ItcVersions](cacheName, cacheVersion, ())
      remote   <- itcClient.versions.attempt
      _        <- (existing, remote.toOption) match {
                    case (Some(ItcVersions(_, a)), Some(ItcVersions(_, b))) if a =!= b =>
                      Logger[F].info("ITC data version mismatch, clear the cache") *> cache.clear
                    case _                                                             =>
                      Applicative[F].unit
                  }
      _        <- cache
                    .eval(cacheableRequest)
                    .apply(())
                    .attempt // Ignore errors
    } yield ()
}
