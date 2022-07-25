// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.model.boopickle.CatalogPicklers
import lucuma.core.model.SiderealTracking

import java.time.Instant
import scala.scalajs.js
import java.time.Duration
import org.http4s.Uri
import explore.modes.SpectroscopyModesMatrix
import lucuma.ags.GuideStarCandidate

package object events {
  object picklers extends CatalogPicklers with EventPicklers

  val LogoutEventId = 1

  sealed trait WorkerMessage

  final case class CatalogRequest(
    tracking: SiderealTracking,
    obsTime:  Instant
  ) extends WorkerMessage

  final case class CatalogResultsMessage(
    results: CatalogResults
  ) extends WorkerMessage

  /**
   * Holds a set of candidate guide stars
   */
  final case class CatalogResults(candidates: List[GuideStarCandidate]) extends WorkerMessage

  /**
   * Error produced by the catalog
   */
  final case class CatalogQueryError(errorMsg: String) extends WorkerMessage

  final case class CacheCleanupRequest(elapsedTime: Duration) extends WorkerMessage

  final case class SpectroscopyMatrixRequest(uri: Uri) extends WorkerMessage

  final case class SpectroscopyMatrixResults(matrix: SpectroscopyModesMatrix) extends WorkerMessage

  // These are messages sent across tabs thus they need to be JS compatible
  // We don't need yet more than just an index to  differentiate
  sealed trait ExploreEvent extends js.Object {
    def event: Int
    def value: js.Any // encode whatever value as a String. it can be e.g. json
  }

  object ExploreEvent {
    class LogoutEvent(val nonce: Long) extends ExploreEvent {
      val event = LogoutEventId
      val value = nonce.toString
    }

    object LogoutEvent {
      val event                               = LogoutEventId
      def apply(nonce: Long)                  = new LogoutEvent(nonce)
      def unapply(l: LogoutEvent): Some[Long] = Some(l.nonce)
    }

  }

}
