// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic.*
import cats.data.*
import eu.timepit.refined.types.numeric.PosInt
import explore.model.boopickle.ItcPicklers
import explore.model.itc.ItcChartResult
import explore.model.itc.ItcQueryProblems
import explore.model.itc.ItcRequestParams
import explore.model.itc.ItcResult
import explore.model.itc.ItcTarget
import explore.modes.InstrumentRow
import explore.modes.SpectroscopyModeRow
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.util.TimeSpan
import lucuma.schemas.model.CentralWavelength
import org.http4s.Uri
import workers.WorkerRequest

object ItcMessage extends ItcPicklers:
  sealed trait Request extends WorkerRequest

  case class Initialize(itcURI: Uri) extends Request:
    type ResponseType = Nothing

  case object CleanCache extends Request:
    type ResponseType = Unit

  case class Query(
    wavelength:      Wavelength,
    signalToNoise:   SignalToNoise,
    constraints:     ConstraintSet,
    targets:         ItcTarget,
    modes:           List[SpectroscopyModeRow],
    signalToNoiseAt: Wavelength
  ) extends Request:
    type ResponseType = Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]]

  case class GraphQuery(
    wavelength:      CentralWavelength,
    exposureTime:    TimeSpan,
    exposures:       PosInt,
    signalToNoiseAt: Wavelength,
    constraints:     ConstraintSet,
    targets:         NonEmptyList[ItcTarget],
    modes:           InstrumentRow
  ) extends Request:
    type ResponseType = Map[ItcTarget, Either[ItcQueryProblems, ItcChartResult]]

  private given Pickler[Query] = generatePickler

  private given Pickler[GraphQuery] = generatePickler

  private given Pickler[Initialize] = generatePickler

  private given Pickler[CleanCache.type] = generatePickler

  given Pickler[Request] = generatePickler
