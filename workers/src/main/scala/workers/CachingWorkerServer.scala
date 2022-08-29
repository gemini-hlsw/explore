// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.~>
import cats.effect.Async
import cats.syntax.all.*
import crystal.react.implicits.*
import boopickle.Pickler
import cats.Monoid
import org.typelevel.log4cats.Logger
import org.scalajs.dom.DedicatedWorkerGlobalScope
import japgolly.scalajs.react.callback.*
import japgolly.webapputil.binary.*
import japgolly.webapputil.boopickle.*
import japgolly.webapputil.indexeddb.IndexedDb.DatabaseName
import japgolly.webapputil.indexeddb.*
import japgolly.webapputil.indexeddb.IndexedDb.OpenCallbacks
import scalajs.js.JSConverters.given
import scalajs.js
import java.time.Instant
import scala.runtime.Tuples

trait CachingWorkerServer[F[_]: Async, T: Pickler](dbName: String)(using
  Monoid[F[Unit]]
) extends WorkerServer[F, T] {

  // Copied and generalized from CallbackCatsEffect - Move somewhere else
  class AsyncCallbackToF(dispatch: Callback => Unit) extends (AsyncCallback ~> F) {
    override def apply[A](f: AsyncCallback[A]): F[A] =
      F.async[A](k =>
        F.delay {
          val s = new AsyncCallback.State
          val g = f.underlyingRepr(s)
          val d = g(t => Callback(k(t.toEither)))
          dispatch(d)
          s.cancelCallback.map(x => F.delay(x.runNow()))
        }
      )
  }

  case class Key(value: Pickled, instant: Instant)

  extension [A](self: AsyncCallback[A])
    def toF: F[A] =
      new AsyncCallbackToF(_.async.toCallback.runNow())(self)

  private val DBVersion: Int = 1

  private val valuesStore =
    ObjectStoreDef.Sync(
      s"$dbName-store",
      KeyCodec[Key](
        key => IndexedDbKey.fromJs(js.Array(key.value.value.toJSArray, key.instant.toEpochMilli)),
        idbKey =>
          CallbackTo {
            val tuple = idbKey.asJs.asInstanceOf[js.Tuple2[js.Array[Byte], Long]]
            Key(Pickled(tuple._1.toArray), Instant.ofEpochMilli(tuple._2))
          }
      ),
      ValueCodec[Pickled](
        s => CallbackTo(s.value.toJSArray),
        s => CallbackTo(Pickled(s.asInstanceOf[js.Array[Byte]].toArray))
      )
    )

  private val openCallbacks = OpenCallbacks(_.createObjectStore(valuesStore, createdInDbVer = 1))

  override protected def handlerInternal(
    self: DedicatedWorkerGlobalScope
  ): Logger[F] ?=> F[Invocation => F[Unit]] =
    for
      idb         <- F.delay(self.indexedDB.toOption.map(new IndexedDb(_)))
      cacheDb     <- idb.traverse(_.open(DatabaseName(dbName), DBVersion)(openCallbacks).toF)
      baseHandler <- handler
    yield invocation =>
      baseHandler(
        Invocation(
          invocation.data,
          invocation.rawData,
          response =>
            invocation.respondRaw(response) >>
              F.delay(Instant.now) >>= (now =>
              cacheDb
                .map(_.put(valuesStore)(Key(invocation.rawData, now), response).toF)
                .orEmpty
            )
        )
      )

}
