// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic.*
import cats.data.*
import explore.model.boopickle.ItcPicklers
import explore.model.itc.ItcAsterismGraphResults
import explore.model.itc.ItcQueryProblem
import explore.model.itc.ItcRequestParams
import explore.model.itc.ItcResult
import explore.model.itc.ItcTarget
import explore.model.itc.ItcTargetProblem
import explore.modes.ItcInstrumentConfig
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Timestamp
import org.http4s.Uri
import workers.WorkerRequest

object ItcMessage extends ItcPicklers:
  sealed trait Request extends WorkerRequest

  case class Initialize(itcURI: Uri) extends Request:
    type ResponseType = Nothing

  case object CleanCache extends Request:
    type ResponseType = Unit

  case class Query(
    exposureTimeMode:    ExposureTimeMode,
    constraints:         ConstraintSet,
    asterism:            NonEmptyList[ItcTarget],
    customSedTimestamps: List[Timestamp],
    modes:               List[ItcInstrumentConfig]
  ) extends Request:
    type ResponseType = Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]]

  case class GraphQuery(
    exposureTimeMode:    ExposureTimeMode,
    constraints:         ConstraintSet,
    asterism:            NonEmptyList[ItcTarget],
    customSedTimestamps: List[Timestamp],
    modes:               ItcInstrumentConfig
  ) extends Request:
    type ResponseType = EitherNec[ItcQueryProblem, ItcAsterismGraphResults]

  private given Pickler[Query] = generatePickler

  private given Pickler[GraphQuery] = generatePickler

  private given Pickler[Initialize] = generatePickler

  private given Pickler[CleanCache.type] = generatePickler

  given Pickler[Request] = generatePickler
