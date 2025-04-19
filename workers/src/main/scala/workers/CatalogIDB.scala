// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.syntax.all.*
import japgolly.scalajs.react.callback.*
import japgolly.webapputil.indexeddb.*
import lucuma.ags.GuideStarCandidate
import lucuma.catalog.votable.*

import java.time.Duration

/**
 * Functions to read and write catalog info from the database. This runs in Callback-land
 */
trait CatalogIDB extends CatalogQuerySettings:

  def readGuideStarCandidates(
    idb:    Option[IndexedDb.Database],
    stores: CacheIDBStores,
    query:  ADQLQuery
  ): AsyncCallback[Option[List[GuideStarCandidate]]] =
    idb
      .map(_.get(stores.candidatesStore)(cacheQueryHash.hash(query)))
      .getOrElse(AsyncCallback.pure(none))

  private def storeGuideStarCandidates(
    idb:     IndexedDb.Database,
    stores:  CacheIDBStores,
    query:   ADQLQuery,
    targets: List[GuideStarCandidate]
  ): AsyncCallback[Unit] =
    val hash = cacheQueryHash.hash(query)

    for {
      ts <- CallbackTo.currentTimeMillis.asAsyncCallback
      // Store a cache entry with a timestamp
      _  <-
        idb.put(stores.cacheStore)(s"gs-candidate-$hash", CacheEntry(ts, hash.toString))
      // Store the results
      _  <-
        idb.put(stores.candidatesStore)(cacheQueryHash.hash(query), targets)
    } yield ()

  def storeGuideStarCandidates(
    idb:     Option[IndexedDb.Database],
    stores:  CacheIDBStores,
    query:   ADQLQuery,
    targets: List[GuideStarCandidate]
  ): AsyncCallback[Unit] =
    idb
      .map(storeGuideStarCandidates(_, stores, query, targets))
      .getOrElse(AsyncCallback.pure(()))

  private def expireGuideStarCandidates(
    idb:        IndexedDb.Database,
    stores:     CacheIDBStores,
    expiration: Duration
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
                  .when((ts - t.timestamp) > expiration.toMillis())
                  .void
              )
            )
        )
    } yield ()

  def expireGuideStarCandidates(
    idb:        Option[IndexedDb.Database],
    stores:     CacheIDBStores,
    expiration: Duration
  ): AsyncCallback[Unit] =
    idb
      .map(expireGuideStarCandidates(_, stores, expiration))
      .getOrElse(AsyncCallback.pure(()))
