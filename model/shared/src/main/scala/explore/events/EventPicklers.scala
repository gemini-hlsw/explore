// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic._
import explore.events.SpectroscopyMatrixResults
import explore.events._
import explore.model.boopickle.CatalogPicklers
import explore.model.boopickle.ItcPicklers
import explore.model.boopickle.ItcPicklers._
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

  private implicit def picklerCatalogRequest: Pickler[CatalogRequest] =
    transformPickler(Function.tupled(CatalogRequest.apply _))(x => (x.tracking, x.vizTime))

  private implicit def picklerCacheCleanupRequest: Pickler[CacheCleanupRequest] =
    transformPickler(CacheCleanupRequest.apply)(_.elapsedTime)

  private implicit def picklerSpectroscopyMatrixRequest: Pickler[SpectroscopyMatrixRequest] =
    transformPickler(SpectroscopyMatrixRequest.apply)(_.uri)

  private implicit def picklerSpectroscopyMatrixResult: Pickler[SpectroscopyMatrixResults] =
    transformPickler(SpectroscopyMatrixResults.apply)(_.matrix)

  implicit def picklerCatalogResults: Pickler[CatalogResults] =
    transformPickler(CatalogResults.apply)(_.candidates)

  private implicit def picklerCatalogQueryError: Pickler[CatalogQueryError] =
    transformPickler(CatalogQueryError.apply)(_.errorMsg)

  private implicit def picklerAgsRequest: Pickler[AgsRequest] =
    transformPickler(AgsRequest.apply.tupled)(x =>
      (x.id, x.constraints, x.wavelength, x.baseCoordinates, x.position, x.params, x.candidates)
    )

  given Pickler[AgsResult] =
    transformPickler(AgsResult.apply)(_.results)

  given Pickler[ItcQuery] =
    transformPickler(ItcQuery.apply.tupled)(x =>
      (x.id, x.wavelength, x.signalToNoise, x.constraints, x.targets, x.modes)
    )

  given Pickler[ItcQueryResult] =
    transformPickler(ItcQueryResult.apply.tupled)(x => (x.id, x.results))

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
      .addConcreteType[ItcQueryResult]

object EventPicklers extends EventPicklers
