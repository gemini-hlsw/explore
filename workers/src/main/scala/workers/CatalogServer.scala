// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import explore.events.AgsMessage
import explore.events.CatalogMessage
import explore.model.boopickle.CatalogPicklers.given
import explore.model.boopickle.CommonPicklers.given
import japgolly.webapputil.indexeddb.IndexedDb
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis
import org.http4s.dom.FetchClientBuilder
import org.scalajs.dom
import org.typelevel.log4cats.Logger

import java.time.Duration
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("CatalogServer", moduleID = "exploreworkers")
object CatalogServer extends WorkerServer[IO, CatalogMessage.Request] with CatalogCache:
  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  // Expire the data in 30 days
  private val Expiration: Duration = Duration.ofDays(30)

  protected val handler: Logger[IO] ?=> IO[Invocation => IO[Unit]] =
    for
      self    <- IO(dom.DedicatedWorkerGlobalScope.self)
      idb     <- IO(self.indexedDB.toOption)
      stores   = CacheIDBStores()
      cacheDb <- idb.traverse(idb => stores.open(IndexedDb(idb)).toF[IO])
      client   = FetchClientBuilder[IO].create
    yield invocation =>
      invocation.data match
        case req @ CatalogMessage.GSRequest(_, _) =>
          readFromGaia(client, cacheDb, stores, req, c => invocation.respond(c)) *>
            expireGuideStarCandidates(cacheDb, stores, Expiration)
              .toF[IO]
              .handleErrorWith(e => Logger[IO].error(e)("Error expiring guidestar candidates"))

        case CatalogMessage.GSCacheCleanupRequest(expTime) =>
          expireGuideStarCandidates(cacheDb, stores, expTime)
            .toF[IO]
            .handleErrorWith(e => Logger[IO].error(e)("Error expiring guidestar candidates"))
