// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import lucuma.core.model.SiderealTracking

import java.time.Instant
import scala.scalajs.js

package object events extends WorkerEncoders {
  val LogoutEventId         = 1
  val CatalogRequestEventId = 2
  val CacheCleanupEventId   = 3
  val CatalogResultsEventId = 4

  // These are messages sent across tabs thus they need to be JS compatible
  // We don't need yet more than just an index to  differentiate
  sealed trait ExploreEvent extends js.Object {
    def event: Int
    def value: js.Any // encode whatever value as a String. it can be e.g. json
  }

  final case class CatalogRequest(tracking: SiderealTracking, obsTime: Instant)

  object CatalogRequest {
    implicit val requestDecoder: Decoder[CatalogRequest] = deriveDecoder[CatalogRequest]
    implicit val requestEncoder: Encoder[CatalogRequest] = deriveEncoder[CatalogRequest]
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

    class CatalogRequestEvent(catalogRequest: CatalogRequest) extends ExploreEvent {
      val event = CatalogRequestEventId
      val value = catalogRequest.asJson.noSpaces
    }

    object CatalogRequestEvent {
      val event                                        = CatalogRequestEventId
      def apply(t: SiderealTracking, obsTime: Instant) = new CatalogRequestEvent(
        CatalogRequest(t, obsTime)
      )
    }

    class CacheCleanupEvent(elapsedTime: Int) extends ExploreEvent {
      val event = CacheCleanupEventId
      val value = elapsedTime
    }

    object CacheCleanupEvent {
      val event                   = CacheCleanupEventId
      def apply(elapsedTime: Int) = new CacheCleanupEvent(elapsedTime)
    }
  }
}
