// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import io.circe.syntax._
import lucuma.core.model.SiderealTracking

import scala.scalajs.js

package object events extends WorkerEncoders {
  val LogoutEvent         = 1
  val CatalogRequestEvent = 2

  // These are messages sent across tabs thus they need to be JS compatible
  // We don't need yet more than just an index to  differentiate
  sealed trait ExploreEvent extends js.Object {
    def event: Int
    def value: js.Any // encode whatever value as a String. it can be e.g. json
  }

  object ExploreEvent {
    class Logout(val nonce: Long) extends ExploreEvent {
      val event = LogoutEvent
      val value = nonce.toString
    }

    object Logout {
      val event                          = LogoutEvent
      def apply(nonce: Long)             = new Logout(nonce)
      def unapply(l: Logout): Some[Long] = Some(l.nonce)
    }

    class CatalogRequest(q: SiderealTracking) extends ExploreEvent {
      val event = CatalogRequestEvent
      val value = q.asJson.noSpaces
    }

    object CatalogRequest {
      val event                      = CatalogRequestEvent
      def apply(t: SiderealTracking) = new CatalogRequest(t)
    }
  }
}
