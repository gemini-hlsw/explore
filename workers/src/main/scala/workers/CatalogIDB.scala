// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import explore.model.CatalogResults
import lucuma.ags.GuideStarCandidate
import japgolly.scalajs.react.callback._
import japgolly.webapputil.indexeddb._
import lucuma.catalog._

/**
 * Functions to read and write catalog info from the database. This runs in Callback-land
 */
trait CatalogIDB extends CatalogQuerySettings {

  def readGuideStarCandidates(
    idb:    IndexedDb.Database,
    stores: CacheIDBStores,
    query:  ADQLQuery
  ): AsyncCallback[Option[CatalogResults]] =
    idb.get(stores.candidatesStore)(cacheQueryHash.hash(query))

  def storeGuideStarCandidates(
    idb:     IndexedDb.Database,
    stores:  CacheIDBStores,
    query:   ADQLQuery,
    targets: List[GuideStarCandidate]
  ): AsyncCallback[Unit] = {
    val hash = cacheQueryHash.hash(query)

    for {
      ts <- CallbackTo.currentTimeMillis.asAsyncCallback
      // Store a cache entry with a timestamp
      _  <-
        idb.put(stores.cacheStore)(s"gs-candidate-$hash", CacheEntry(ts, hash.toString))
      // Store the results
      _  <-
        idb.put(stores.candidatesStore)(cacheQueryHash.hash(query), CatalogResults(targets))
    } yield ()
  }

  def expireGuideStarCandidates(
    idb:        IndexedDb.Database,
    stores:     CacheIDBStores,
    expiration: Double
  ): AsyncCallback[Unit] =
    for {
      ts   <- CallbackTo.currentTimeMillis.asAsyncCallback
      keys <- idb.getAllKeys(stores.cacheStore)
      _    <-
        AsyncCallback.traverse(keys)(x =>
          idb
            .get(stores.cacheStore)(x)
            .flatMap(u =>
              u.fold(AsyncCallback.unit)(t =>
                (idb.delete(stores.cacheStore)(x) *>
                  idb.delete(stores.candidatesStore)(t.key.toInt))
                  .when((ts - t.timestamp) > expiration)
                  .void
              )
            )
        )
    } yield ()

}
