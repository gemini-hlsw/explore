// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.syntax.all._
import cats.data.EitherNec
import cats.effect.Concurrent
import cats.effect.IO
import cats.Hash
import explore.model.CatalogResults
import explore.model.CatalogQueryError
import lucuma.ags.GuideStarCandidate
import lucuma.core.model.Target
import explore.events.picklers._
import lucuma.core.geom.gmos.probeArm
import japgolly.webapputil.indexeddb._
import lucuma.ags
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
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.temporal.ChronoField
import java.time.temporal.ChronoUnit
import spire.math.Bounded
import explore.events.CatalogRequest
import lucuma.core.enums._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.math.Wavelength
import explore.model.Constants

trait CatalogQuerySettings {
  val proxy = uri"https://lucuma-cors-proxy.herokuapp.com"

  val MaxTargets = 30000

  implicit val coordinatesHash: Hash[Coordinates] = Hash.fromUniversalHashCode
  implicit val catalog                            = CatalogAdapter.Gaia3Lite
  implicit val ci                                 = ADQLInterpreter.nTarget(MaxTargets)

  def cacheQueryHash: Hash[ADQLQuery] =
    Hash.by(q => (MaxTargets, catalog.gaiaDB, q.base, q.adqlGeom, q.adqlBrightness))

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
          .through(CatalogSearch.guideStars[F](CatalogAdapter.Gaia3Lite))
      )
      .compile
      .toList
  }

  // We'll use the same constraints that includes all the AGS queries
  val constraints = ConstraintSet(
    ImageQuality.PointOne,         // min image quality
    CloudExtinction.PointOne,      // min cloud extinction
    SkyBackground.Dark,            // Not relevant
    WaterVapor.Wet,                // Not relevant
    ElevationRange.AirMass.Default // Not relevant
  )

  // Min relevant wavelength at 300nm
  val wavelength = Wavelength.fromNanometers(300).get

  /**
   * Try to read the gaia query from the cache or else get it from gaia
   */
  def readFromGaia(
    client:     Client[IO],
    self:       dom.DedicatedWorkerGlobalScope,
    idb:        IndexedDb.Database,
    stores:     CacheIDBStores,
    request:    CatalogRequest
  )(implicit L: Logger[IO]): IO[Unit] = {

    val CatalogRequest(tracking, obsTime) = request

    val brightnessConstraints = ags.widestConstraints

    val ldt   = LocalDateTime.ofInstant(obsTime, ZoneId.of("UTC"))
    // We consider the query valid from the fist moment of the year to the end
    val start =
      ldt.`with`(ChronoField.DAY_OF_YEAR, 1L).`with`(ChronoField.NANO_OF_DAY, 0)
    val end   = start.plus(1, ChronoUnit.YEARS)

    // Make a time based query for pm over a year
    val query = TimeRangeQueryByADQL(
      tracking,
      Bounded(start.toInstant(Constants.UTCOffset), end.toInstant(Constants.UTCOffset), 0),
      probeArm.candidatesArea,
      brightnessConstraints.some,
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
            .handleErrorWith { e =>
              postAsTransferable[IO, CatalogQueryError](self, CatalogQueryError(e.getMessage()))
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
