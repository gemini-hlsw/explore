// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic._
import explore.events.SpectroscopyMatrixResults
import explore.events._
import explore.model.boopickle.CatalogPicklers
import explore.model.boopickle.ItcPicklers._
import lucuma.ags.AgsParams
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import org.http4s.Uri

import java.time.Duration
import java.time.Instant
import lucuma.core.model.Target

/**
 * Picklers used by web workers
 */
trait EventPicklers extends CatalogPicklers {
  implicit def picklerInstant: Pickler[Instant] =
    transformPickler(Instant.ofEpochMilli)(_.toEpochMilli())

  implicit def picklerDuration: Pickler[Duration] =
    transformPickler(Duration.ofMillis)(_.toMillis)

  implicit def picklerUri: Pickler[Uri] =
    transformPickler(Uri.unsafeFromString)(_.toString)

  private implicit def picklerCatalogRequest: Pickler[CatalogRequest] =
    transformPickler(Function.tupled(CatalogRequest.apply _))(x => (x.tracking, x.vizTime))

  private implicit def picklerCacheCleanupRequestt: Pickler[CacheCleanupRequest] =
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
    transformPickler(
      (x: Tuple7[
        Target.Id,
        ConstraintSet,
        Wavelength,
        Coordinates,
        AgsPosition,
        AgsParams,
        List[GuideStarCandidate],
      ]) =>
        x match {
          case (
                id,
                constraints,
                wavelength,
                baseCoordinates,
                position,
                params,
                candidates
              ) =>
            AgsRequest(id, constraints, wavelength, baseCoordinates, position, params, candidates)
        }
    )(x =>
      (
        x.id,
        x.constraints,
        x.wavelength,
        x.baseCoordinates,
        x.position,
        x.params,
        x.candidates
      )
    )

  private implicit def picklerAgsResult: Pickler[AgsResult] =
    transformPickler(AgsResult.apply)(_.results)

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
}

object EventPicklers extends EventPicklers
