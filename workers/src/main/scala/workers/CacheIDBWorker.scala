// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.effect.IO
import cats.effect.kernel.Sync
import cats.effect.unsafe.implicits._
import cats.syntax.all._
import explore.events._
import explore.model.boopickle._
import japgolly.scalajs.react.callback.CallbackCatsEffect._
import japgolly.scalajs.react.callback._
import japgolly.webapputil.indexeddb.IndexedDb
import log4cats.loglevel.LogLevelLogger
import org.http4s.dom.FetchClientBuilder
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import typings.loglevel.mod.LogLevelDesc

import scala.scalajs.js

import js.annotation._

trait AsyncToIO {
  class AsyncCallbackOps[A](val a: AsyncCallback[A]) {
    def toIO: IO[A] = asyncCallbackToIO.apply(a)
  }

  implicit def AsyncTo[A](a: AsyncCallback[A]): AsyncCallbackOps[A] =
    new AsyncCallbackOps(a)
}

/**
 * Web worker that can query gaia and store results locally
 */
@JSExportTopLevel("CacheIDBWorker", moduleID = "cacheworker")
object CacheIDBWorker extends CatalogCache with EventPicklers with AsyncToIO {

  @JSExport
  def runWorker(): Unit = run.handleError(t => t.printStackTrace()).unsafeRunAndForget()

  def setupLogger[F[_]: Sync]: F[Logger[F]] = Sync[F].delay {
    LogLevelLogger.setLevel(LogLevelDesc.INFO)
    LogLevelLogger.createForRoot[F]
  }

  // Expire the data in 30 days
  val Expiration: Double = 30 * 24 * 60 * 60 * 1000.0

  def run: IO[Unit] =
    for {
      logger      <- setupLogger[IO]
      self        <- IO(dom.DedicatedWorkerGlobalScope.self)
      idb         <- IO(self.indexedDB.get)
      stores       = CacheIDBStores()
      cacheDb     <- stores.open(IndexedDb(idb)).toIO
      (client, _) <- FetchClientBuilder[IO].allocated
      _           <-
        IO {
          self.onmessage = (msg: dom.MessageEvent) =>
            // Decode transferrable events
            decodeFromTransferable[CatalogRequest](msg)
              .map { case req @ CatalogRequest(_, _) =>
                readFromGaia(client, self, cacheDb, stores, req)(
                  logger
                ) *> expireGuideStarCandidates(cacheDb, stores, Expiration).toIO
              }
              .orElse(decodeFromTransferable[CacheCleanupRequest](msg).map {
                case CacheCleanupRequest(expTime) =>
                  expireGuideStarCandidates(cacheDb, stores, expTime.toDouble).toIO
              })
              .orEmpty
              .unsafeRunAndForget()
        }
    } yield ()
}
