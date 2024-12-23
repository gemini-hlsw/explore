// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import boopickle.Pickler
import explore.model.boopickle.CatalogPicklers.given
import japgolly.scalajs.react.callback.*
import japgolly.webapputil.binary.*
import japgolly.webapputil.boopickle.*
import japgolly.webapputil.indexeddb.*
import japgolly.webapputil.indexeddb.IndexedDb.DatabaseName
import lucuma.ags.GuideStarCandidate
import japgolly.webapputil.boopickle.BinaryFormatExt.Implicits.*

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
  import CacheIDBStores.{DBVersion, dbName}

  // Initialises or upgrades the IndexedDB database
  protected def onUpgradeNeeded(c: IndexedDb.VersionChange): Callback

  // object-store to store catalog results indexed by the hash of the query
  // Async as it is binary
  val candidatesStore: ObjectStoreDef.Async[Int, List[GuideStarCandidate]]

  // object-store for storing the cache indexes and expiration date
  val cacheStore: ObjectStoreDef.Sync[String, CacheEntry]

  final def clean(idb: IndexedDb.Database): AsyncCallback[Unit] =
    idb.clear(candidatesStore) *> idb.clear(cacheStore)

  final def open(idb: IndexedDb): AsyncCallback[IndexedDb.Database] =
    idb.open(dbName, DBVersion)(IndexedDb.OpenCallbacks(onUpgradeNeeded))
}

object CacheIDBStores {
  val dbName = DatabaseName("explore")

  val DBVersion = 1

  // Data type picklers
  object DBPicklers {
    import SafePickler.ConstructionHelperImplicits.*

    //  SafePickler` is defined in webapp-util and provides some additional features.
    implicit def safePicklerCatalogResults: SafePickler[List[GuideStarCandidate]] =
      summon[Pickler[List[GuideStarCandidate]]]
        .asV1(0) // This is v1.0 of our data format.
        // Add some header/footer coming frow webapputil example
        .withMagicNumbers(0x8cf0655b, 0x5a8218eb)
  }

  def apply(): CacheIDBStores = new CacheIDBStores {
    import DBPicklers.*

    // Store the cache expiration and key
    override val cacheStore =
      ObjectStoreDef.Sync(
        "explore-cache",
        KeyCodec.string,
        ValueCodec[CacheEntry](s => CallbackTo(s), s => CallbackTo(s.asInstanceOf[CacheEntry]))
      )

    // ObjectStore for storing our guide star candidates
    override val candidatesStore: ObjectStoreDef.Async[Int, List[GuideStarCandidate]] = {

      def valueFormat: BinaryFormat[List[GuideStarCandidate]] =
        // Declare that we want to support binary format evolution.
        BinaryFormat.versioned(
          // v1.0: Use implicit SafePickler[CatalogResults]
          BinaryFormat.id.pickle[List[GuideStarCandidate]]
          // Our hypothetical future v1.1 protocol would be here
        )

      def valueCodec: ValueCodec.Async[List[GuideStarCandidate]] =
        ValueCodec.Async.binary(valueFormat)

      ObjectStoreDef.Async("gs-candidates", KeyCodec.int, valueCodec)
    }

    override protected def onUpgradeNeeded(c: IndexedDb.VersionChange): Callback =
      Callback.runAll(
        c.createObjectStore(cacheStore, createdInDbVer = 1),
        c.createObjectStore(candidatesStore, createdInDbVer = 1)
      )
  }
}
