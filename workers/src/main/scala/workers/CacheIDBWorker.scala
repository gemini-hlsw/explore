// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.effect._
import cats.effect.unsafe.implicits._
import cats.syntax.all._
import clue.TransactionalClient
import clue._
import clue.js.FetchJSBackend
import clue.js.FetchMethod
import explore.events._
import explore.itc.ITCGraphRequests
import explore.itc.ITCRequests
import explore.model.AppConfig
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
import queries.schemas.ITC
import typings.loglevel.mod.LogLevelDesc

import java.time.Duration
import scala.concurrent.duration._
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
    LogLevelLogger.setLevel(LogLevelDesc.DEBUG)
    LogLevelLogger.createForRoot[F]
  }

  // Expire the data in 30 days
  val Expiration: Duration = Duration.ofDays(30)

  def fetchConfig[F[_]: Async]: F[AppConfig] =
    // We want to avoid caching the static server redirect and the config files (they are not fingerprinted by vite).
    AppConfig.fetchConfig(
      FetchClientBuilder[F]
        .withRequestTimeout(5.seconds)
        .withCache(dom.RequestCache.`no-store`)
        .create
    )

  def run: IO[Unit] =
    for {
      given Logger[IO]                   <- setupLogger[IO]
      self                               <- IO(dom.DedicatedWorkerGlobalScope.self)
      idb                                <- IO(self.indexedDB.get)
      stores                              = CacheIDBStores()
      cacheDb                            <- stores.open(IndexedDb(idb)).toIO
      matrix                             <- Deferred[IO, SpectroscopyModesMatrix]
      client                              = FetchClientBuilder[IO].create
      config                             <- fetchConfig[IO]
      given TransactionalClient[IO, ITC] <- {
        given TransactionalBackend[IO] = FetchJSBackend[IO](FetchMethod.GET)
        TransactionalClient.of[IO, ITC](config.itcURI, "ITC")
      }
      _                                  <-
        IO {
          val logger = summon[Logger[IO]]

          self.onmessage = (msg: dom.MessageEvent) =>
            // Decode transferrable events
            decodeFromTransferable[WorkerMessage](msg)
              .map(_ match {
                case req @ CatalogRequest(_, _) =>
                  readFromGaia(client, self, cacheDb, stores, req)(logger) *>
                    expireGuideStarCandidates(cacheDb, stores, Expiration).toIO

                case SpectroscopyMatrixRequest(uri) =>
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

                case CacheCleanupRequest(expTime) =>
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

                case ItcQuery(id, wavelength, signalToNoise, constraint, targets, rows) =>
                  logger.debug(s"ITC query ${rows.length}") *>
                    ITCRequests
                      .queryItc[IO](wavelength,
                                    signalToNoise,
                                    constraint,
                                    targets,
                                    rows,
                                    r => postWorkerMessage[IO](self, ItcQueryResult(id, r))
                      )

                case ItcGraphQuery(id, wavelength, signalToNoise, constraint, targets, mode) =>
                  logger.debug(s"ITC graph query ${mode}") *>
                    ITCGraphRequests
                      .queryItc[IO](wavelength,
                                    signalToNoise,
                                    constraint,
                                    targets,
                                    mode,
                                    r => postWorkerMessage[IO](self, ItcGraphResult(id, r))
                      )

                case _ => IO.unit
              })
              .orEmpty
              .handleError(_.printStackTrace())
              .unsafeRunAndForget()
        }
    } yield ()
}
