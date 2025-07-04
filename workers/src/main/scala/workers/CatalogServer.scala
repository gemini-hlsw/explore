// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import explore.events.CatalogMessage
import explore.model.boopickle.CatalogPicklers.given
import japgolly.webapputil.indexeddb.IndexedDb
import lucuma.catalog.clients.GaiaClient
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.ObservingModeType
import lucuma.core.geom.flamingos2
import lucuma.core.geom.gmos
import org.http4s.dom.FetchClientBuilder
import org.scalajs.dom
import org.typelevel.log4cats.Logger

import java.time.Duration
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("CatalogServer", moduleID = "exploreworkers")
object CatalogServer extends WorkerServer[IO, CatalogMessage.Request] with CatalogCache:
  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  // Expire the data in 30 days
  private val Expiration: Duration           = Duration.ofDays(30)
  private val RequestTimeout: FiniteDuration = FiniteDuration(300, TimeUnit.SECONDS)

  protected val handler: Logger[IO] ?=> IO[Invocation => IO[Unit]] =
    for
      self      <- IO(dom.DedicatedWorkerGlobalScope.self)
      idb       <- IO(self.indexedDB.toOption)
      stores     = CacheIDBStores()
      cacheDb   <- idb.traverse(idb => stores.open(IndexedDb(idb)).toF[IO])
      httpClient = FetchClientBuilder[IO].withRequestTimeout(RequestTimeout).create
      client     = GaiaClient.build(httpClient)
    yield invocation =>
      invocation.data match
        case CatalogMessage.CleanCache                     =>
          cacheDb.traverse(stores.clean(_).toF[IO]).void
        case req @ CatalogMessage.GSRequest(_, _, obsMode) =>
          val candidatesArea = obsMode match {
            case ObservingModeType.GmosNorthLongSlit | ObservingModeType.GmosSouthLongSlit |
                ObservingModeType.GmosNorthImaging | ObservingModeType.GmosSouthImaging =>
              gmos.candidatesArea.candidatesArea
            case ObservingModeType.Flamingos2LongSlit =>
              // In practice lyot is always F16
              flamingos2.candidatesArea.candidatesArea(
                Flamingos2LyotWheel.F16
              )
          }
          readFromGaia(client, cacheDb, stores, req, candidatesArea, c => invocation.respond(c)) *>
            expireGuideStarCandidates(cacheDb, stores, Expiration)
              .toF[IO]
              .handleErrorWith(e => Logger[IO].error(e)("Error expiring guidestar candidates"))

        case CatalogMessage.GSCacheCleanupRequest(expTime) =>
          expireGuideStarCandidates(cacheDb, stores, expTime)
            .toF[IO]
            .handleErrorWith(e => Logger[IO].error(e)("Error expiring guidestar candidates"))
