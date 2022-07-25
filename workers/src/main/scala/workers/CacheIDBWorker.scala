// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.effect.IO
import cats.effect.Sync
import cats.effect.Deferred
import cats.effect.unsafe.implicits._
import cats.syntax.all._
import explore.events._
import explore.model.boopickle._
import explore.model.StaticData
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
import java.time.Duration
import explore.modes.SpectroscopyModesMatrix

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
  val Expiration: Duration = Duration.ofDays(30)

  def run: IO[Unit] =
    for {
      logger  <- setupLogger[IO]
      self    <- IO(dom.DedicatedWorkerGlobalScope.self)
      idb     <- IO(self.indexedDB.get)
      stores   = CacheIDBStores()
      cacheDb <- stores.open(IndexedDb(idb)).toIO
      matrix  <- Deferred[IO, SpectroscopyModesMatrix]
      client   = FetchClientBuilder[IO].create
      _       <-
        IO {
          self.onmessage = (msg: dom.MessageEvent) => {
            println(s"msg ")
            // Decode transferrable events
            println(decodeFromTransferable[WorkerMessage](msg))
            decodeFromTransferable[WorkerMessage](msg)
              .map(_ match {
                case req @ CatalogRequest(_, _)     =>
                  println("REQ")
                  readFromGaia(client, self, cacheDb, stores, req)(logger) *>
                    expireGuideStarCandidates(cacheDb, stores, Expiration).toIO
                case SpectroscopyMatrixRequest(uri) =>
                  implicit val log = logger
                  StaticData.build[IO](uri).flatMap { m =>
                    matrix.complete(m) /* *>
                        postAsTransferable[IO, SpectroscopyMatrixResults](
                          self,
                          SpectroscopyMatrixResults(m)
                        )*/
                  } *> IO.println(uri)
                case CacheCleanupRequest(expTime)   =>
                  println("Cleanup")
                  expireGuideStarCandidates(cacheDb, stores, expTime).toIO
                case _                              => IO.unit
              })
              .orEmpty
              .handleError(_.printStackTrace())
              .unsafeRunAndForget()
          }
        }
    } yield ()
}
