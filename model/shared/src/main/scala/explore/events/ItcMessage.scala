// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic.*
import cats.data.*
import explore.model.boopickle.ItcPicklers
import explore.model.itc.ItcAsterismGraphResults
import explore.model.itc.ItcRequestParams
import explore.model.itc.ItcResult
import explore.model.itc.ItcTarget
import explore.model.itc.ItcTargetProblem
import explore.modes.InstrumentConfig
import explore.modes.SpectroscopyModeRow
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
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
    asterism:        NonEmptyList[ItcTarget],
    modes:           List[SpectroscopyModeRow],
    signalToNoiseAt: Wavelength
  ) extends Request:
    type ResponseType = Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]]

  case class GraphQuery(
    wavelength:      CentralWavelength,
    signalToNoise:   SignalToNoise,
    signalToNoiseAt: Wavelength,
    constraints:     ConstraintSet,
    asterism:        NonEmptyList[ItcTarget],
    modes:           InstrumentConfig
  ) extends Request:
    type ResponseType = ItcAsterismGraphResults

  private given Pickler[Query] = generatePickler

  private given Pickler[GraphQuery] = generatePickler

  private given Pickler[Initialize] = generatePickler

  private given Pickler[CleanCache.type] = generatePickler

  given Pickler[Request] = generatePickler
