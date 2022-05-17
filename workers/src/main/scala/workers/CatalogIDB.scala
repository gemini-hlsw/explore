// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.Hash
import cats.syntax.all._
import explore.events._
import explore.model.CatalogResults
import explore.model.GuideStarCandidate
import io.circe.scalajs._
import japgolly.scalajs.react.callback._
import japgolly.webapputil.indexeddb.IndexedDb.DatabaseName
import japgolly.webapputil.indexeddb.IndexedDb.OpenCallbacks
import japgolly.webapputil.indexeddb._
import lucuma.catalog._
import lucuma.core.geom.jts.interpreter._
import lucuma.core.math.Coordinates
import org.scalajs.dom

import java.time.Instant
import scala.annotation.nowarn
import scala.scalajs.js

@js.native
@nowarn
trait CacheEntry extends js.Object {
  val timestamp: Double = js.native
  val key: String       = js.native
}

object CacheEntry {
  def apply(timestamp: Long, key: String): CacheEntry =
    js.Dynamic.literal(timestamp = timestamp.toDouble, key = key).asInstanceOf[CacheEntry]
}

/**
 * Functions to read and write catalog info from the database. This runs in Callback-land
 */
trait CatalogIDB {
  implicit val coordinatesHash: Hash[Coordinates] = Hash.fromUniversalHashCode
  implicit val ci                                 = ADQLInterpreter.nTarget(10000)

  def cacheQueryHash: Hash[ADQLQuery] = Hash.by(q => (q.base, q.adqlGeom, q.adqlBrightness))

  val DBVersion = 1

  val candidatesStore = ObjectStoreDef.Async(
    "gs-candidates",
    KeyCodec.int,
    ValueCodec[CatalogResults](
      s => CallbackTo(s.asJsAny),
      s =>
        CallbackTo(decodeJs[CatalogResults](s.asInstanceOf[js.Any]).getOrElse(CatalogResults.empty))
    ).async
  )

  // Store the cache expiration and key
  val cacheStore = ObjectStoreDef.Async(
    "explore-cache",
    KeyCodec.string,
    ValueCodec[CacheEntry](
      s => CallbackTo(s),
      s => CallbackTo(s.asInstanceOf[CacheEntry])
    ).async
  )

  def unusedOpenCallbacks: OpenCallbacks =
    OpenCallbacks(
      upgradeNeeded = _ => Callback.empty,
      versionChange = _ => Callback.empty,
      closed = Callback.empty
    )

  def createStoresOnOpen(stores: ObjectStoreDef[_, _]*): OpenCallbacks =
    unusedOpenCallbacks.copy(
      upgradeNeeded = e => Callback.traverse(stores)(e.db.createObjectStore(_))
    )

  def openDB(self: dom.DedicatedWorkerGlobalScope): AsyncCallback[IndexedDb.Database] =
    AsyncCallback
      .delay(self.indexedDB.get)
      .flatMap(idb =>
        IndexedDb(idb)
          .open(DatabaseName("explore"), DBVersion)(
            createStoresOnOpen(List(candidatesStore, cacheStore): _*)
          )
      )

  def readGuideStarCandidates(
    idb:   IndexedDb.Database,
    query: ADQLQuery
  ): AsyncCallback[Option[CatalogResults]] =
    idb.get(candidatesStore)(cacheQueryHash.hash(query))

  def storeGuideStarCandidates(
    idb:     IndexedDb.Database,
    query:   ADQLQuery,
    targets: List[GuideStarCandidate]
  ): AsyncCallback[Unit] = {
    val hash = cacheQueryHash.hash(query)

    for {
      ts <- CallbackTo.currentTimeMillis.asAsyncCallback
      // Store a timestamp
      _  <-
        idb.put(cacheStore)(s"gs-candidate-$hash", CacheEntry(ts, hash.toString))
      // Store the results
      _  <-
        idb.put(candidatesStore)(cacheQueryHash.hash(query), CatalogResults(targets))
    } yield ()
  }

  def expireGuideStarCandidates(
    idb:        IndexedDb.Database,
    expiration: Double
  ): AsyncCallback[Unit] =
    for {
      ts   <- CallbackTo.currentTimeMillis.asAsyncCallback
      keys <- idb.getAllKeys(cacheStore)
      _    <-
        AsyncCallback.traverse(keys)(x =>
          idb
            .get(cacheStore)(x)
            .flatMap(u =>
              u.fold(AsyncCallback.unit)(t =>
                (idb.delete(cacheStore)(x) *>
                  idb.delete(candidatesStore)(t.key.toInt))
                  .when((ts - t.timestamp) > expiration)
                  .void
              )
            )
        )
    } yield ()

}
