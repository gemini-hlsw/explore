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
import log4cats.loglevel.LogLevelLogger
import lucuma.catalog._
import lucuma.core.geom.gmos.probeArm
import lucuma.core.model.Target
import org.http4s.Method._
import org.http4s.Request
import org.http4s.client.Client
import org.http4s.dom.FetchClientBuilder
import org.http4s.syntax.all._
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import typings.loglevel.mod.LogLevelDesc

import scala.scalajs.js

import js.annotation._
import java.time.temporal.ChronoUnit
import spire.math.Bounded
import java.time.temporal.ChronoField
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.ZoneOffset

/**
 * Sample web worker, extremely simple just accept messages, prints them and answers a number
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
  def runWorker(): Unit = run.unsafeRunAndForget()

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

  def setupLogger[F[_]: Sync]: F[Logger[F]] = Sync[F].delay {
    LogLevelLogger.setLevel(LogLevelDesc.DEBUG)
    LogLevelLogger.createForRoot[F]
  }

  val UTC       = ZoneId.of("UTC")
  val UTCOffset = ZoneOffset.UTC

  def run: IO[Unit] =
    for {
      logger      <- setupLogger[IO]
      self        <- IO(dom.DedicatedWorkerGlobalScope.self)
      idb         <- openDB(self).toIO
      (client, _) <- FetchClientBuilder[IO].allocated
      _           <- IO {
                       self.onmessage = (msg: dom.MessageEvent) => {
                         val event = msg.data.asInstanceOf[ExploreEvent]
                         event.event match {
                           case ExploreEvent.CatalogRequestEvent.event =>
                             decode[ExploreEvent.CatalogRequest](event.value.toString).foreach {
                               case ExploreEvent.CatalogRequest(tracking, obsTime) =>
                                 val ldt   = LocalDateTime.ofInstant(obsTime, ZoneId.of("UTC"))
                                 // We consider the query valid from the fist moment of the year to the end
                                 val start =
                                   ldt.`with`(ChronoField.DAY_OF_YEAR, 1L).`with`(ChronoField.NANO_OF_DAY, 0)
                                 val end   = start.plus(1, ChronoUnit.YEARS)

                                 val query = TimeRangeQueryByADQL(
                                   tracking,
                                   Bounded(start.toInstant(UTCOffset), end.toInstant(UTCOffset), 0),
                                   probeArm.candidatesArea,
                                   bc.some,
                                   proxy.some
                                 )

                                 (logger.debug(s"Requested catalog $query ${cacheQueryHash.hash(query)}") *>
                                   readStoredTargets(idb, query).toIO) // Try to find it in the db
                                   .flatMap(
                                     _.fold(
                                       // Not found in the db, re requset
                                       readFromGaia[IO](client, query)
                                         .map(
                                           _.collect { case Right(s) =>
                                             GuideStarCandidate.siderealTarget.get(s)
                                           }
                                         )
                                         .flatMap { candidates =>
                                           logger.debug(
                                             s"Got catalog results from remote catalog: ${candidates.length} candidates"
                                           ) *>
                                             IO(self.postMessage(candidates.asJson.noSpaces)) *>
                                             storeTargets(idb, query, candidates).toIO
                                         }
                                     )(c =>
                                       // Cache hit!
                                       logger.debug(
                                         s"Got catalog results from cache: ${c.candidates.length} candidates"
                                       ) *> IO(
                                         self.postMessage(c.candidates.asJson.noSpaces)
                                       )
                                     )
                                   )
                                   .unsafeRunAndForget()
                             }

                           case _ =>
                         }
                       }
                     }
    } yield ()
}
