// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import cats.Eq
import cats.data._
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.model.boopickle.CatalogPicklers
import explore.model.itc.ItcChart
import explore.model.itc.ItcQueryProblems
import explore.model.itc.ItcRequestParams
import explore.model.itc.ItcResult
import explore.model.itc.ItcTarget
import explore.modes.InstrumentRow
import explore.modes.SpectroscopyModeRow
import explore.modes.SpectroscopyModesMatrix
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import org.http4s.Uri

import java.time.Duration
import java.time.Instant
import java.util.UUID

object picklers extends CatalogPicklers with EventPicklers

sealed trait WorkerMessage

final case class CatalogRequest(
  tracking: SiderealTracking,
  vizTime:  Instant
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

final case class AgsRequest(
  id:              Target.Id,
  constraints:     ConstraintSet,
  wavelength:      Wavelength,
  baseCoordinates: Coordinates,
  position:        AgsPosition,
  params:          AgsParams,
  candidates:      List[GuideStarCandidate]
) extends WorkerMessage

final case class AgsResult(results: List[AgsAnalysis]) extends WorkerMessage

final case class ItcQuery(
  id:            UUID,
  wavelength:    Wavelength,
  signalToNoise: PosBigDecimal,
  constraints:   ConstraintSet,
  targets:       NonEmptyList[ItcTarget],
  modes:         List[SpectroscopyModeRow]
) extends WorkerMessage

final case class ItcGraphQuery(
  id:            UUID,
  wavelength:    Wavelength,
  signalToNoise: PosBigDecimal,
  constraints:   ConstraintSet,
  targets:       NonEmptyList[ItcTarget],
  modes:         InstrumentRow
) extends WorkerMessage

final case class ItcQueryResult(
  id:      UUID,
  results: Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]]
) extends WorkerMessage

final case class ItcGraphResult(
  id:      UUID,
  results: List[ItcChart]
) extends WorkerMessage
