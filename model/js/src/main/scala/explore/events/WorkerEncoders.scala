// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic._
import explore.model.boopickle.CatalogPicklers._
import explore.events._
import java.time.Instant

/**
 * Picklers used by web workers
 */
trait WorkerPicklers {
  implicit def picklerInstant: Pickler[Instant] =
    transformPickler(Instant.ofEpochMilli)(_.toEpochMilli())

  implicit def picklerCatalogRequest: Pickler[CatalogRequest] =
    transformPickler(Function.tupled(CatalogRequest.apply _))(x => (x.tracking, x.obsTime))

  implicit def picklerCacheCleanupRequestt: Pickler[CacheCleanupRequest] =
    transformPickler(CacheCleanupRequest.apply)(_.elapsedTime)

}

object WorkerPicklers extends WorkerPicklers
