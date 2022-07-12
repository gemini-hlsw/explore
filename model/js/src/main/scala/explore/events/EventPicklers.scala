// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic._
import explore.events._
import explore.model.boopickle.CatalogPicklers._

import java.time.Instant
import java.time.Duration

/**
 * Picklers used by web workers
 */
trait EventPicklers {
  implicit def picklerInstant: Pickler[Instant] =
    transformPickler(Instant.ofEpochMilli)(_.toEpochMilli())

  implicit def picklerDuration: Pickler[Duration] =
    transformPickler(Duration.ofMillis)(_.toMillis)

  implicit def picklerCatalogRequest: Pickler[CatalogRequest] =
    transformPickler(Function.tupled(CatalogRequest.apply _))(x => (x.tracking, x.obsTime))

  implicit def picklerCacheCleanupRequestt: Pickler[CacheCleanupRequest] =
    transformPickler { (r: Duration) => println(s"READ $r"); CacheCleanupRequest(r) } { c =>
      println(c); c.elapsedTime
    }

  implicit def picklerSpectroscopyMatrixRequest: Pickler[SpectroscopyMatrixRequest] =
    transformPickler(Function.tupled(SpectroscopyMatrixRequest.apply _))(x => (x.fileName, x.i))

}

object EventPicklers extends EventPicklers
