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
import lucuma.core.geom.jts.interpreter._
import lucuma.core.model.SiderealTracking
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

/**
 * Sample web worker, extremely simple just accept messages, prints them and answers a number
 */
@JSExportTopLevel("CatalogWorker", moduleID = "catalogworker")
object CatalogWorker extends CatalogIDB {
  val proxy       = uri"https://lucuma-cors-proxy.herokuapp.com"
  implicit val ci = ADQLInterpreter.nTarget(10000)
  // TODO Read this from a table
  val bc          =
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
    query:  QueryByADQL
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
                           case ExploreEvent.CatalogRequest.event =>
                             decode[SiderealTracking](event.value.toString).foreach { tracking =>
                               val query = QueryByADQL(tracking.baseCoordinates,
                                                       probeArm.candidatesArea,
                                                       bc.some,
                                                       proxy.some
                               )

                               (logger.debug(s"Requested catalog $query ${cacheQueryHash.hash(query)}") *>
                                 readStoredTargets(idb, query).toIO)
                                 .flatMap(
                                   _.fold(
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
