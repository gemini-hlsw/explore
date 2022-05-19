// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import lucuma.core.model.SiderealTracking

import java.time.Instant
import scala.scalajs.js
import explore.model.boopickle.CatalogPicklers

package object events {
  object picklers extends CatalogPicklers with WorkerPicklers

  val LogoutEventId         = 1
  val CatalogRequestEventId = 2
  val CacheCleanupEventId   = 3

  // These are messages sent across tabs thus they need to be JS compatible
  // We don't need yet more than just an index to  differentiate
  sealed trait ExploreEvent extends js.Object {
    def event: Int
    def value: js.Any // encode whatever value as a String. it can be e.g. json
  }

  final case class CatalogRequest(tracking: SiderealTracking, obsTime: Instant)

  final case class CacheCleanupRequest(elapsedTime: Int)

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
