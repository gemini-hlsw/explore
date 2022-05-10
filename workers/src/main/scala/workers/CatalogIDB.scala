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
import lucuma.core.math.Coordinates
import org.scalajs.dom

import java.time.Instant
import scala.scalajs.js

/**
 * Functions to read and write catalog info from the database. This runs in Callback-land
 */
trait CatalogIDB {
  implicit val coordinatesHash: Hash[Coordinates] = Hash.fromUniversalHashCode
  val cacheQueryHash: Hash[QueryByADQL]           = Hash.by(q => (q.base, q.adqlBrightness, q.adqlBrightness))

  val DBVersion = 1

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
          .open(DatabaseName("gaia"), DBVersion)(createStoresOnOpen(List(store): _*))
      )

  val store = ObjectStoreDef.Async(
    "targets",
    KeyCodec.int,
    ValueCodec[CatalogResults](
      s => CallbackTo(s.asJsAny),
      s =>
        CallbackTo(decodeJs[CatalogResults](s.asInstanceOf[js.Any]).getOrElse(CatalogResults.empty))
    ).async
  )

  def readStoredTargets(
    idb:   IndexedDb.Database,
    query: QueryByADQL
  ): AsyncCallback[Option[CatalogResults]] =
    idb.get(store)(cacheQueryHash.hash(query))

  def storeTargets(
    idb:     IndexedDb.Database,
    query:   QueryByADQL,
    targets: List[GuideStarCandidate]
  ): AsyncCallback[Unit] =
    idb.add(store)(cacheQueryHash.hash(query), CatalogResults(targets, Instant.now()))

}
