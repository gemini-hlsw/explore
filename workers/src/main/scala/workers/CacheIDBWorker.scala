// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Sync
import cats.effect.unsafe.implicits._
import cats.syntax.all._
import explore.events._
import explore.model.StaticData
import explore.model.boopickle.Boopickle._
import explore.modes.SpectroscopyModesMatrix
import japgolly.scalajs.react.callback.CallbackCatsEffect._
import japgolly.scalajs.react.callback._
import japgolly.webapputil.indexeddb.IndexedDb
import log4cats.loglevel.LogLevelLogger
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis
import org.http4s.dom.FetchClientBuilder
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import typings.loglevel.mod.LogLevelDesc

import java.time.Duration
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
          self.onmessage = (msg: dom.MessageEvent) =>
            // Decode transferrable events
            decodeFromTransferable[WorkerMessage](msg)
              .map(_ match {
                case req @ CatalogRequest(_, _)                                                 =>
                  readFromGaia(client, self, cacheDb, stores, req)(logger) *>
                    expireGuideStarCandidates(cacheDb, stores, Expiration).toIO
                case SpectroscopyMatrixRequest(uri)                                             =>
                  implicit val log = logger
                  matrix.tryGet.flatMap {
                    case Some(m) =>
                      logger.debug("ITC matrix load from memory") *>
                        postWorkerMessage[IO](self, SpectroscopyMatrixResults(m))
                    case _       =>
                      logger.debug("ITC matrix load from remote") *>
                        StaticData.build[IO](uri).flatMap { m =>
                          matrix.complete(m) *>
                            postWorkerMessage[IO](self, SpectroscopyMatrixResults(m))
                        }
                  }
                case CacheCleanupRequest(expTime)                                               =>
                  expireGuideStarCandidates(cacheDb, stores, expTime).toIO
                case AgsRequest(id, constraints, wavelength, base, basePos, params, candidates) =>
                  logger.debug(s"AGS request for $id") *>
                    IO.delay(
                      Ags
                        .agsAnalysis(constraints, wavelength, base, basePos, params, candidates)
                        .sorted(AgsAnalysis.rankingOrdering)
                    ).flatMap(r =>
                      logger.debug(s"AGS response for $id") *>
                        postWorkerMessage[IO](self, AgsResult(r))
                    )
                case _                                                                          => IO.unit
              })
              .orEmpty
              .handleError(_.printStackTrace())
              .unsafeRunAndForget()
        }
    } yield ()
}
