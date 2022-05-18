// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.Hash
import explore.model.CatalogResults
import explore.model.CatalogPicklers
import explore.model.GuideStarCandidate
import japgolly.scalajs.react.callback._
import japgolly.webapputil.indexeddb.IndexedDb.DatabaseName
import japgolly.webapputil.indexeddb._
import japgolly.webapputil.binary._
import japgolly.webapputil.boopickle._
import lucuma.catalog._
import lucuma.core.geom.jts.interpreter._
import lucuma.core.math.Coordinates

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

trait CacheIDBStores {
  import CacheIDBStores.{ DBVersion, dbName }

  // Initialises or upgrades the IndexedDB database
  protected def onUpgradeNeeded(c: IndexedDb.VersionChange): Callback

  // object-store to store catalog results indexed by the hash of the query
  // This is an async store because we'll compress data before storing it;
  val candidatesStore: ObjectStoreDef.Async[Int, CatalogResults]

  // object-store for storing the cache indexes and expiration date
  val cacheStore: ObjectStoreDef.Sync[String, CacheEntry]

  // Convenience function to open a connection to the IndexedDB database
  final def open(idb: IndexedDb): AsyncCallback[IndexedDb.Database] =
    idb.open(dbName, DBVersion)(IndexedDb.OpenCallbacks(onUpgradeNeeded))
}

object CacheIDBStores {
  val dbName = DatabaseName("explore")

  val DBVersion = 1

  // Here we'll define how to convert from our data types to IndexedDB values and back.
  object DBPicklers extends CatalogPicklers {
    import SafePickler.ConstructionHelperImplicits._

    // Where `Pickler` comes from Boopickle, `SafePickler` is defined in webapp-util and provides some additional features.
    implicit def safePicklerPerson: SafePickler[CatalogResults] =
      picklerCatalogResults
        .asV1(0) // This is v1.0 of our data format.
        // Add some header/footer valueResultss for a bit of extra integrity.
        .withMagicNumbers(0x8cf0655b, 0x5a8218eb)
  }

  def apply(): CacheIDBStores = new CacheIDBStores {
    import DBPicklers._

    // Store the cache expiration and key
    override val cacheStore                                                 = ObjectStoreDef.Sync(
      "explore-cache",
      KeyCodec.string,
      ValueCodec[CacheEntry](
        s => CallbackTo(s),
        s => CallbackTo(s.asInstanceOf[CacheEntry])
      )
    )
    //
    // ObjectStore for storing our guide star candidates
    override val candidatesStore: ObjectStoreDef.Async[Int, CatalogResults] = {

      // Our all-things-considered binary format for storing Person instances
      def valueFormat: BinaryFormat[CatalogResults] =
        // Declare that we want to support binary format evolution.
        BinaryFormat.versioned(
          // v1.0: Use implicit SafePickler[Person]
          BinaryFormat.id.pickle[CatalogResults]
          // Our hypothetical future v1.1 protocol would be here
        )

      // Convert from Person to raw JS IndexedDB values
      def valueCodec: ValueCodec.Async[CatalogResults] =
        ValueCodec.Async.binary(valueFormat)

      // Finally we create the store definition itself
      ObjectStoreDef.Async("gs-candidates", KeyCodec.int, valueCodec)
    }

    override protected def onUpgradeNeeded(c: IndexedDb.VersionChange): Callback =
      // This is where we initialise/migrate the database.
      Callback.runAll(
        c.createObjectStore(cacheStore, createdInDbVer = 1),
        c.createObjectStore(candidatesStore, createdInDbVer = 1)
      )
  }
}

/**
 * Functions to read and write catalog info from the database. This runs in Callback-land
 */
trait CatalogIDB {
  // We need an instance of JS library `Pako` for compression
  // implicit def pako: Pako = Pako.global

  implicit val coordinatesHash: Hash[Coordinates] = Hash.fromUniversalHashCode
  implicit val ci                                 = ADQLInterpreter.nTarget(10000)

  def cacheQueryHash: Hash[ADQLQuery] = Hash.by(q => (q.base, q.adqlGeom, q.adqlBrightness))

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
