// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import boopickle.Pickler
import cats.Applicative
import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import cats.syntax.all.*
import explore.model.boopickle.Boopickle.*
import japgolly.scalajs.react.callback.*
import japgolly.webapputil.indexeddb.IndexedDb
import japgolly.webapputil.indexeddb.IndexedDb.Database
import japgolly.webapputil.indexeddb.IndexedDbKey
import japgolly.webapputil.indexeddb.KeyCodec
import japgolly.webapputil.indexeddb.ObjectStoreDef
import japgolly.webapputil.indexeddb.ValueCodec
import lucuma.utils.NewType
import org.scalajs.dom
import org.scalajs.dom.CacheStorage
import org.scalajs.dom.IDBFactory
import org.scalajs.dom.{Cache => JsCache}

import java.time.Duration
import java.time.Instant
import java.util.Base64

import scalajs.js
import scalajs.js.JSConverters.*
import scalajs.js.typedarray.Int8Array

object CacheName extends NewType[String]
type CacheName = CacheName.Type

object CacheVersion extends NewType[Int]
type CacheVersion = CacheVersion.Type

/**
 * Cacheable computation.
 */
case class Cacheable[F[_], I, O](name: CacheName, version: CacheVersion, invoke: I => F[O])

/**
 * Generic cache interface.
 */
sealed trait Cache[F[_]](using F: Sync[F]):
  def eval[I: Pickler, O: Pickler](computation: Cacheable[F, I, O]): I => F[O]
  def evict(until:                              Instant): F[Unit]
  def evict(retention: Duration): F[Unit] =
    F.delay(Instant.now) >>= (now => evict(now.minus(retention)))

/**
 * `NoCache` is used when, for some reason, a cache could not be initialized.
 */
case class NoCache[F[_]]()(using F: Sync[F]) extends Cache[F]:
  override def eval[I: Pickler, O: Pickler](computation: Cacheable[F, I, O]): I => F[O] =
    computation.invoke

  override def evict(until: Instant): F[Unit] = F.unit

/**
 * Cache backed up by an Indexed DB store.
 */
case class IDBCache[F[_]](
  cacheDB:   IndexedDb.Database,
  store:     ObjectStoreDef.Sync[Pickled, Pickled],
  dbFactory: IDBFactory, // Only needed for low-level access for eviction
  dbName:    String,     // Only needed for low-level access for eviction
  dbVersion: Int         // Only needed for low-level access for eviction
)(using
  F:         Async[F]
) extends Cache[F]:
  override def eval[I: Pickler, O: Pickler](computation: Cacheable[F, I, O]): I => F[O] = { input =>
    val pickledInput: Pickled = Pickled(
      asBytes((computation.name.value, computation.version.value, input))
    )

    cacheDB
      .get(store)(pickledInput)
      .toF
      .flatMap(
        _.fold(
          computation
            .invoke(input)
            .flatTap(output => cacheDB.put(store)(pickledInput, Pickled(asBytes(output))).toF)
        )(pickledOutput => F.pure(fromBytes[O](pickledOutput.value)).rethrow)
      )
      .handleErrorWith { t =>
        F.delay(t.printStackTrace); throw t
      }
  }

  override def evict(until: Instant): F[Unit] =
    F.async_ { cb =>
      val dbReq = dbFactory.open(dbName, dbVersion)

      dbReq.onerror = e => cb((new Exception(e.message)).asLeft)
      dbReq.onsuccess = { e =>
        val db = e.target.result

        val transaction = db.transaction(js.Array(store.name), dom.IDBTransactionMode.readwrite)
        val objectStore = transaction.objectStore(store.name)

        val cursorReq = objectStore.openCursor() // Iterate through all entries

        cursorReq.onerror = e => cb((new Exception(e.message)).asLeft)
        cursorReq.onsuccess = { e =>
          val cursor = e.target.result

          if (cursor != null) {
            val value     = cursor.value.asInstanceOf[js.Tuple3[js.Array[Byte], Int, Int]]
            val timestamp = Instant.ofEpochSecond(value._2, value._3)

            if (timestamp.isBefore(until))
              objectStore.delete(cursor.key)

            cursor.continue()
          } else {
            db.close()
            cb(().asRight)
          }
        }
      }
    }

/**
 * Cache backed up by a JS `Cache`.
 */
case class JsCacheCache[F[_]: PromiseConverter](cache: JsCache)(using F: Sync[F]) extends Cache[F]:
  def eval[I: Pickler, O: Pickler](computation: Cacheable[F, I, O]): I => F[O] = { input =>
    val pickledInput: Array[Byte] = asBytes(input)
    val base64Input: String       = Base64.getEncoder.encode(pickledInput).map(_.toChar).mkString
    val stringInput: String       =
      s"http://request/${computation.name}/v${computation.version}?$base64Input"

    PromiseConverter[F]
      .convert(cache.`match`(stringInput))
      .flatMap(
        _.fold(
          computation
            .invoke(input)
            .flatTap(output =>
              PromiseConverter[F].convert(
                cache.put(stringInput, new dom.Response(asTypedArray(output)))
              )
            )
        )(response =>
          PromiseConverter[F]
            .convert(response.arrayBuffer())
            .flatMap(ab => F.pure(fromTypedArray[O](new Int8Array(ab))).rethrow)
        )
      )
  }

  override def evict(until: Instant): F[Unit] = F.unit

object Cache:
  def withIDB[F[_]: Async](dbFactory: Option[IDBFactory], dbName: String): F[Cache[F]] =
    dbFactory.fold(Sync[F].delay(NoCache[F]())) { factory =>
      val storeName: String = s"$dbName-store"

      val DBVersion: Int = 1

      val store: ObjectStoreDef.Sync[Pickled, Pickled] = ObjectStoreDef.Sync(
        storeName,
        KeyCodec[Pickled](
          i => IndexedDbKey.fromJs(i.value.toJSArray),
          k => CallbackTo(Pickled(k.asJs.asInstanceOf[js.Array[Byte]].toArray))
        ),
        ValueCodec[Pickled](
          o =>
            CallbackTo(Instant.now) >>= (i =>
              CallbackTo(js.Tuple3(o.value.toJSArray, i.getEpochSecond.toInt, i.getNano))
            ),
          v => CallbackTo(Pickled(v.asInstanceOf[js.Tuple3[js.Array[Byte], Int, Int]]._1.toArray))
        )
      )

      IndexedDb(factory)
        .open(IndexedDb.DatabaseName(dbName), DBVersion)(
          IndexedDb.OpenCallbacks(upgradeNeeded =
            _.createObjectStore(store, createdInDbVer = DBVersion)
          )
        )
        .toF
        .map(db => IDBCache[F](db, store, factory, dbName, DBVersion))
    }

  def withJsCache[F[_]: Async: PromiseConverter](
    cacheStorage: Option[CacheStorage],
    cacheName:    String
  ): F[Cache[F]] =
    cacheStorage.fold(Sync[F].delay(NoCache[F]())) { caches =>
      PromiseConverter[F]
        .convert(caches.open(cacheName))
        .map(cache => JsCacheCache(cache))
    }
