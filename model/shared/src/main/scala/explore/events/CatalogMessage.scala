// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic.*
import boopickle.Pickler
import explore.model.boopickle.CatalogPicklers
import lucuma.ags.GuideStarCandidate
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.ObjectTracking
import workers.WorkerRequest

import java.time.Duration
import java.time.Instant

object CatalogMessage extends CatalogPicklers {
  sealed trait Request   extends WorkerRequest
  case object CleanCache extends Request {
    type ResponseType = Unit
  }

  case class GSRequest(
    tracking:    ObjectTracking,
    vizTime:     Instant,
    obsModeType: ObservingModeType
  ) extends Request {
    type ResponseType = List[GuideStarCandidate]
  }

  case class GSCacheCleanupRequest(elapsedTime: Duration) extends Request {
    type ResponseType = Nothing
  }

  private given Pickler[GSRequest] = generatePickler

  private given Pickler[GSCacheCleanupRequest] = generatePickler

  given Pickler[CleanCache.type] = generatePickler

  given Pickler[Request] = generatePickler
}
