// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.data.EitherNec
import cats.effect.Concurrent
import cats.effect.IO
import cats.effect.kernel.Sync
import cats.effect.unsafe.implicits._
import cats.syntax.all._
import explore.events._
import explore.model.GuideStarCandidate
import fs2.text
import io.circe.parser._
import io.circe.syntax._
import japgolly.scalajs.react.callback.CallbackCatsEffect._
import japgolly.scalajs.react.callback._
import japgolly.webapputil.indexeddb.IndexedDb
import japgolly.webapputil.boopickle._
import japgolly.webapputil.binary._
import log4cats.loglevel.LogLevelLogger
import lucuma.catalog._
import lucuma.core.geom.gmos.probeArm
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import org.http4s.Method._
import org.http4s.Request
import org.http4s.client.Client
import org.http4s.dom.FetchClientBuilder
import org.http4s.syntax.all._
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import spire.math.Bounded
import typings.loglevel.mod.LogLevelDesc

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.temporal.ChronoField
import java.time.temporal.ChronoUnit
import scala.scalajs.js

import js.annotation._

/**
 * Web worker that can query gaia and store results locally
 */
@JSExportTopLevel("CatalogWorker", moduleID = "catalogworker")
object CatalogWorker extends CatalogIDB {
  val proxy = uri"https://lucuma-cors-proxy.herokuapp.com"
  // TODO Read this from a table
  val bc    =
    BrightnessConstraints(BandsList.GaiaBandsList,
                          FaintnessConstraint(16),
                          SaturationConstraint(9).some
    )

  class AsyncCallbackOps[A](val a: AsyncCallback[A]) {
    def toIO: IO[A] = asyncCallbackToIO.apply(a)
  }

  implicit def AsyncTo[A](a: AsyncCallback[A]): AsyncCallbackOps[A] =
    new AsyncCallbackOps(a)

  @JSExport
  def runWorker(): Unit = run.handleError(t => t.printStackTrace()).unsafeRunAndForget()

  /**
   * Request and parse data from Gaia
   */
  def readFromGaia[F[_]: Concurrent](
    client: Client[F],
    query:  ADQLQuery
  ): F[List[EitherNec[CatalogProblem, Target.Sidereal]]] = {
    val queryUri = CatalogSearch.gaiaSearchUri(query)
    val request  = Request[F](GET, queryUri)
    client
      .stream(request)
      .flatMap(
        _.body
          .through(text.utf8.decode)
          .through(CatalogSearch.guideStars[F](CatalogAdapter.Gaia))
      )
      .compile
      .toList
  }

  /**
   * Try to read the gaia query from the cache or else get it from gaia
   */
  def readFromGaia(
    client:     Client[IO],
    self:       dom.DedicatedWorkerGlobalScope,
    idb:        IndexedDb.Database,
    stores:     CacheIDBStores,
    tracking:   SiderealTracking,
    obsTime:    Instant
  )(implicit L: Logger[IO]): IO[Unit] = {
    val ldt   = LocalDateTime.ofInstant(obsTime, ZoneId.of("UTC"))
    // We consider the query valid from the fist moment of the year to the end
    val start =
      ldt.`with`(ChronoField.DAY_OF_YEAR, 1L).`with`(ChronoField.NANO_OF_DAY, 0)
    val end   = start.plus(1, ChronoUnit.YEARS)

    // Make a time based query for pm over a year
    val query = TimeRangeQueryByADQL(
      tracking,
      Bounded(start.toInstant(UTCOffset), end.toInstant(UTCOffset), 0),
      probeArm.candidatesArea,
      bc.some,
      proxy.some
    )

    (L.debug(s"Requested catalog $query ${cacheQueryHash.hash(query)}") *>
      readGuideStarCandidates(idb, stores, query).toIO.handleError(_ =>
        none
      )) // Try to find it in the db
      .flatMap(
        _.fold(
          // Not found in the db, re request
          readFromGaia[IO](client, query)
            .map(
              _.collect { case Right(s) =>
                GuideStarCandidate.siderealTarget.get(s)
              }
            )
            .flatMap { candidates =>
              L.debug(s"Catalog results from remote catalog: ${candidates.length} candidates") *>
                IO(self.postMessage(candidates.asJson.noSpaces)) *>
                storeGuideStarCandidates(idb, stores, query, candidates).toIO
                  .handleError(e => L.error(e)("Error storing guidstar candidates"))
            }
            .void
        )(c =>
          // Cache hit!
          L.debug(s"Catalog results from cache: ${c.candidates.length} candidates") *>
            IO(self.postMessage(c.candidates.asJson.noSpaces))
        )
      )
  }

  def setupLogger[F[_]: Sync]: F[Logger[F]] = Sync[F].delay {
    LogLevelLogger.setLevel(LogLevelDesc.DEBUG)
    LogLevelLogger.createForRoot[F]
  }

  val UTC                = ZoneId.of("UTC")
  val UTCOffset          = ZoneOffset.UTC
  // Expire the data in 30 days
  val Expiration: Double = 30 * 24 * 60 * 60 * 1000.0

  // 4) We need an encryption key
  val encKey = BinaryData.fromStringAsUtf8("!" * 32)

  def run: IO[Unit] =
    for {
      logger      <- setupLogger[IO]
      _           <- logger.info(s"Worker starting ")
      self        <- IO(dom.DedicatedWorkerGlobalScope.self)
      idb         <- IO(self.indexedDB.get)
      stores       = CacheIDBStores()
      cacheDb     <- stores.open(IndexedDb(idb)).toIO
      (client, _) <- FetchClientBuilder[IO].allocated
      _           <-
        IO {
          self.onmessage = (msg: dom.MessageEvent) => {
            val event = msg.data.asInstanceOf[ExploreEvent]
            event.event match {
              case ExploreEvent.CatalogRequestEvent.event =>
                decode[CatalogRequest](event.value.toString).foreach {
                  case CatalogRequest(tracking, obsTime) =>
                    (readFromGaia(client, self, cacheDb, stores, tracking, obsTime)(
                      logger
                    ) *> expireGuideStarCandidates(cacheDb, stores, Expiration).toIO)
                      .unsafeRunAndForget()
                }

              case ExploreEvent.CacheCleanupEvent.event =>
                expireGuideStarCandidates(cacheDb, stores, Expiration).toIO.unsafeRunAndForget()
              case _                                    =>
            }
          }
        }
    } yield ()
}
