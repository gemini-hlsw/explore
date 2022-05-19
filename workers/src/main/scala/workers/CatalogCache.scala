// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.syntax.all._
import cats.data.EitherNec
import cats.effect.Concurrent
import cats.effect.IO
import cats.Hash
import explore.model.CatalogResults
import explore.model.GuideStarCandidate
import lucuma.core.model.Target
import explore.events.picklers._
import lucuma.core.geom.gmos.probeArm
import japgolly.webapputil.indexeddb._
import lucuma.core.model.SiderealTracking
import lucuma.catalog._
import fs2.text
import org.http4s.Method._
import org.http4s.Request
import org.http4s.client.Client
import org.http4s.syntax.all._
import lucuma.core.geom.jts.interpreter._
import lucuma.core.math.Coordinates
import org.typelevel.log4cats.Logger
import explore.model.boopickle._

import org.scalajs.dom
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.temporal.ChronoField
import java.time.temporal.ChronoUnit
import spire.math.Bounded

trait CatalogQuerySettings {
  val proxy = uri"https://lucuma-cors-proxy.herokuapp.com"
  // TODO Read this from a table
  val bc    =
    BrightnessConstraints(BandsList.GaiaBandsList,
                          FaintnessConstraint(16),
                          SaturationConstraint(9).some
    )

  implicit val coordinatesHash: Hash[Coordinates] = Hash.fromUniversalHashCode
  implicit val ci                                 = ADQLInterpreter.nTarget(10000)

  def cacheQueryHash: Hash[ADQLQuery] = Hash.by(q => (q.base, q.adqlGeom, q.adqlBrightness))

  val UTC       = ZoneId.of("UTC")
  val UTCOffset = ZoneOffset.UTC
}

/**
 * Handles the catalog cache, it tries to use the local db and if not it goes to gaia to get the
 * data
 */
trait CatalogCache extends CatalogIDB with AsyncToIO {

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
      // Try to find it in the db
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
                postAsTransferable[IO, CatalogResults](self, CatalogResults(candidates)) *>
                storeGuideStarCandidates(idb, stores, query, candidates).toIO
                  .handleError(e => L.error(e)("Error storing guidstar candidates"))
            }
            .void
        ) { c =>
          // Cache hit!
          L.debug(s"Catalog results from cache: ${c.candidates.length} candidates") *>
            postAsTransferable[IO, CatalogResults](self, c)
        }
      )
  }

}
