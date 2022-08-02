// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import cats.Eq
import explore.model.boopickle.CatalogPicklers
import explore.modes.SpectroscopyModesMatrix
import lucuma.ags.GuideStarCandidate
import lucuma.core.model.SiderealTracking
import org.http4s.Uri

import java.time.Duration
import java.time.Instant

object picklers extends CatalogPicklers with EventPicklers

sealed trait WorkerMessage

final case class CatalogRequest(
  tracking: SiderealTracking,
  obsTime:  Instant
) extends WorkerMessage

/**
 * Holds a set of candidate guide stars
 */
final case class CatalogResults(candidates: List[GuideStarCandidate]) extends WorkerMessage

object CatalogResults {
  implicit val eqCatalogResults: Eq[CatalogResults] = Eq.by(_.candidates)
}

/**
 * Error produced by the catalog
 */
final case class CatalogQueryError(errorMsg: String) extends WorkerMessage

final case class CacheCleanupRequest(elapsedTime: Duration) extends WorkerMessage

final case class SpectroscopyMatrixRequest(uri: Uri) extends WorkerMessage

final case class SpectroscopyMatrixResults(matrix: SpectroscopyModesMatrix) extends WorkerMessage
