// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic._
import cats.Eq
import cats.data._
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.model.boopickle.CatalogPicklers
import explore.model.boopickle.ItcPicklers
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
import workers.WorkerRequest

import java.time.Duration
import java.time.Instant
import java.util.UUID

object ItcMessage extends ItcPicklers {
  sealed trait Request extends WorkerRequest

  final case class SpectroscopyMatrixRequest(uri: Uri) extends Request {
    type ResponseType = SpectroscopyModesMatrix
  }

  // final case class SpectroscopyMatrixResults(matrix: SpectroscopyModesMatrix)

  final case class Query(
    wavelength:    Wavelength,
    signalToNoise: PosBigDecimal,
    constraints:   ConstraintSet,
    targets:       NonEmptyList[ItcTarget],
    modes:         List[SpectroscopyModeRow]
  ) extends Request {
    type ResponseType = Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]]
  }

  final case class GraphQuery(
    wavelength:    Wavelength,
    signalToNoise: PosBigDecimal,
    constraints:   ConstraintSet,
    targets:       NonEmptyList[ItcTarget],
    modes:         InstrumentRow
  ) extends Request {
    type ResponseType = List[ItcChart]
  }

  private given Pickler[SpectroscopyMatrixRequest] = generatePickler

  private given Pickler[Query] = generatePickler

  private given Pickler[GraphQuery] = generatePickler

  given Pickler[Request] = generatePickler
}
