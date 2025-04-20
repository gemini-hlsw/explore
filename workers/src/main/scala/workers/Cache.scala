// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import boopickle.Pickler
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
import lucuma.core.util.NewType
import org.scalajs.dom
import org.scalajs.dom.IDBFactory

import java.time.Duration
import java.time.Instant
import scala.annotation.unused

import scalajs.js
import scalajs.js.JSConverters.*

object CacheName extends NewType[String]
type CacheName = CacheName.Type

object CacheVersion extends NewType[Int]
type CacheVersion = CacheVersion.Type

/**
 * Cacheable computation.
 */
case class Cacheable[F[_], I, O](
  name:    CacheName,
  version: CacheVersion,
  invoke:  I => F[O],
  doStore: (I, O) => Boolean = (_: I, _: O) => true
)

/**
 * Generic cache interface.
 */
sealed trait Cache[F[_]](using F: Sync[F]):
  def get[I: Pickler, O: Pickler](name: CacheName, version: CacheVersion, key: I): F[Option[O]]

  def eval[I: Pickler, O: Pickler](computation: Cacheable[F, I, O]): I => F[O]

  def evict(until: Instant): F[Unit]

  def evict(retention: Duration): F[Unit] =
    F.delay(Instant.now) >>= (now => evict(now.minus(retention)))

  def clear: F[Unit] = F.unit

/**
 * `NoCache` is used when, for some reason, a cache could not be initialized.
 */
case class NoCache[F[_]]()(using F: Sync[F]) extends Cache[F]:
  @unused
  override def get[I: Pickler, O: Pickler](
    name:    CacheName,
    version: CacheVersion,
    key:     I
  ): F[Option[O]] = none.pure[F]

  @unused
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

  override def get[I: Pickler, O: Pickler](
    name:    CacheName,
    version: CacheVersion,
    key:     I
  ): F[Option[O]] = {
    val pickledKey: Pickled = Pickled(asBytes((name.value, version.value, key)))

    cacheDB
      .get(store)(pickledKey)
      .toF
      .attempt
      .map(_.toOption.flatten) // Treat errors as cache misses
      .map(
        _.fold(
          none
        )(pickledOutput => fromBytes[O](pickledOutput.value).toOption)
      )
  }

  override def eval[I: Pickler, O: Pickler](computation: Cacheable[F, I, O]): I => F[O] = { input =>
    val pickledInput: Pickled = Pickled(
      asBytes((computation.name.value, computation.version.value, input))
    )

    cacheDB
      .get(store)(pickledInput)
      .toF
      .attempt
      .map(_.toOption.flatten) // Treat errors as cache misses
      .flatMap(
        _.fold(
          computation
            .invoke(input)
            .flatTap(output =>
              cacheDB
                .put(store)(pickledInput, Pickled(asBytes(output)))
                .toF
                .whenA(computation.doStore(input, output))
                .handleError(_ => ()) // Ignore errors
            )
        )(pickledOutput => F.pure(fromBytes[O](pickledOutput.value)).rethrow)
      )
  }

  override def clear: F[Unit] =
    cacheDB.clear(store).toF

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
              ()

            cursor.continue()
          } else {
            db.close()
            cb(().asRight)
          }
        }
      }
    }

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
          v =>
            CallbackTo(
              Pickled(v.asInstanceOf[js.Tuple3[js.Array[Byte], Int, Int]]._1.toArray)
            )
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
