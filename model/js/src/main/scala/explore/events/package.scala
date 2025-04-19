// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import fs2.dom.Serializer

import scala.scalajs.js

// These are messages sent across tabs thus they need to be JS compatible
// We don't need yet more than just an index to differentiate
sealed trait ExploreEvent extends js.Object {
  def event: Int
  def value: js.Any // encode whatever value as a String. it can be e.g. json
}

object ExploreEvent {
  given Serializer[ExploreEvent] =
    Serializer.any.imap(_.asInstanceOf[ExploreEvent])(identity(_))

  val LogoutEventId    = 1
  val PWAUpdateId      = 2
  val PWAReloadId      = 3
  val ExploreUIReadyId = 4

  class LogoutEvent(val nonce: String) extends ExploreEvent {
    val event = LogoutEventId
    val value = nonce
  }

  object LogoutEvent {
    val event                                   = LogoutEventId
    def apply(nonce: String)                    = new LogoutEvent(nonce)
    def unapply(l:   LogoutEvent): Some[String] = Some(l.nonce)
  }

  object PWAUpdate extends ExploreEvent {
    val event = PWAUpdateId
    val value = ""
  }

  object PWAReload extends ExploreEvent {
    val event = PWAReloadId
    val value = ""
  }

  object ExploreUIReady extends ExploreEvent {
    val event = ExploreUIReadyId
    val value = ""
  }

}
