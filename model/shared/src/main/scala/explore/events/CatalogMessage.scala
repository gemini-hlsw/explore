// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic._
import boopickle.Pickler
import explore.model.boopickle.CatalogPicklers
import lucuma.ags.GuideStarCandidate
import lucuma.core.model.SiderealTracking
import workers.WorkerRequest

import java.time.Duration
import java.time.Instant

object CatalogMessage extends CatalogPicklers {
  sealed trait Request extends WorkerRequest

  final case class GSRequest(
    tracking: SiderealTracking,
    vizTime:  Instant
  ) extends Request {
    type ResponseType = List[GuideStarCandidate]
  }

  final case class GSCacheCleanupRequest(elapsedTime: Duration) extends Request {
    type ResponseType = Nothing
  }

  private given Pickler[GSRequest] = generatePickler

  private given Pickler[GSCacheCleanupRequest] = generatePickler

  given Pickler[Request] = generatePickler
}
