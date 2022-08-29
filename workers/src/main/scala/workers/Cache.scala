// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import org.scalajs.dom
import org.scalajs.dom.CacheStorage
import org.scalajs.dom.IDBFactory
import org.scalajs.dom.{Cache => JsCache}

import java.time.Instant
import java.util.Base64
import scala.scalajs.js.typedarray.Int8Array

import scalajs.js
import scalajs.js.JSConverters.*

/**
 * Cacheable computation.
 */
case class Cacheable[F[_], I, O](name: String, version: Int, invoke: I => F[O])

/**
 * Generic cache interface.
 */
sealed trait Cache[F[_]]:
  def eval[I: Pickler, O: Pickler](computation: Cacheable[F, I, O]): I => F[O]

/**
 * `NoCache` is used when, for some reason, a cache could not be initialized.
 */
case class NoCache[F[_]]() extends Cache[F]:
  def eval[I: Pickler, O: Pickler](computation: Cacheable[F, I, O]): I => F[O] =
    computation.invoke

/**
 * Cache backed up by an Indexed DB store.
 */
case class IDBCache[F[_]](
  cacheDB: IndexedDb.Database,
  store:   ObjectStoreDef.Sync[Pickled, Pickled]
)(using
  F:       Async[F]
) extends Cache[F]:
  override def eval[I: Pickler, O: Pickler](computation: Cacheable[F, I, O]): I => F[O] = { input =>
    val pickledInput: Pickled = Pickled(asBytes((computation.name, computation.version, input)))

    // F.delay(println(s"CACHED EVAL $input")) >>
    cacheDB
      .get(store)(pickledInput)
      .toF
      // .flatTap(pickled => F.delay(println(pickled.fold("CACHE MISS")(_ => "CACHE HIT"))))
      .flatMap(
        _.fold(
          computation
            .invoke(input)
            .flatTap(output => cacheDB.put(store)(pickledInput, Pickled(asBytes(output))).toF)
        )(pickledOutput => F.pure(fromBytes[O](pickledOutput.value)).rethrow)
      )
    // .flatTap(o => F.delay(println(s"CACHED RESULT $o")))
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
      // .flatTap(pickled => F.delay(println(pickled.fold("CACHE MISS")(_ => "CACHE HIT"))))
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
        .open(IndexedDb.DatabaseName(storeName), DBVersion)(
          IndexedDb.OpenCallbacks(upgradeNeeded =
            _.createObjectStore(store, createdInDbVer = DBVersion)
          )
        )
        .toF
        .map(db => IDBCache[F](db, store))
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
