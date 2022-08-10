// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic._
import explore.events.SpectroscopyMatrixResults
import explore.events._
import explore.model.boopickle.CatalogPicklers
import explore.model.boopickle.ItcPicklers
import explore.model.boopickle.ItcPicklers._
import explore.model.itc.ItcChart
import explore.model.itc.ItcTarget
import lucuma.ags.AgsParams
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Target
import org.http4s.Uri

import java.time.Duration
import java.time.Instant

/**
 * Picklers used by web workers
 */
trait EventPicklers extends CatalogPicklers with ItcPicklers:
  implicit def picklerInstant: Pickler[Instant] =
    transformPickler(Instant.ofEpochMilli)(_.toEpochMilli())

  implicit def picklerDuration: Pickler[Duration] =
    transformPickler(Duration.ofMillis)(_.toMillis)

  implicit def picklerUri: Pickler[Uri] =
    transformPickler(Uri.unsafeFromString)(_.toString)

  private given Pickler[CatalogRequest] = generatePickler

  private given Pickler[CacheCleanupRequest] = generatePickler

  private given Pickler[SpectroscopyMatrixRequest] = generatePickler

  private given Pickler[SpectroscopyMatrixResults] = generatePickler

  implicit def picklerCatalogResults: Pickler[CatalogResults] =
    transformPickler(CatalogResults.apply)(_.candidates)

  private given Pickler[CatalogQueryError] = generatePickler

  private given Pickler[AgsRequest] = generatePickler

  given Pickler[AgsResult] = generatePickler

  given Pickler[ItcQuery] = generatePickler

  given Pickler[ItcGraphQuery] = generatePickler

  given Pickler[ItcChart] = generatePickler

  given Pickler[ItcGraphResult] = generatePickler

  given Pickler[ItcQueryResult] = generatePickler

  implicit val messagePickler: Pickler[WorkerMessage] =
    compositePickler[WorkerMessage]
      .addConcreteType[CatalogRequest]
      .addConcreteType[CacheCleanupRequest]
      .addConcreteType[SpectroscopyMatrixRequest]
      .addConcreteType[SpectroscopyMatrixResults]
      .addConcreteType[CatalogResults]
      .addConcreteType[CatalogQueryError]
      .addConcreteType[AgsRequest]
      .addConcreteType[AgsResult]
      .addConcreteType[ItcQuery]
      .addConcreteType[ItcGraphQuery]
      .addConcreteType[ItcGraphResult]
      .addConcreteType[ItcQueryResult]

object EventPicklers extends EventPicklers
