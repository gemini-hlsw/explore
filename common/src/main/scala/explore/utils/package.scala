// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.scalajs.js

package object utils {

  def abbreviate(s: String, maxLength: Int): String =
    if (s.length > maxLength) s"${s.substring(0, maxLength)}\u2026" else s

  implicit class ListOps[A](val list: List[A]) extends AnyVal {
    def modFirstWhere(find: A => Boolean, mod: A => A): List[A] =
      list.indexWhere(find) match {
        case -1 => list
        case n  => (list.take(n) :+ mod(list(n))) ++ list.drop(n + 1)
      }
  }
}

package utils {

  // These are messages sent across tabs thus they need to be JS compatible
  // We don't need yet more than just an index to  differentiate
  sealed trait ExploreEvent extends js.Object {
    def event: Int
  }

  object ExploreEvent {
    object Logout extends ExploreEvent {
      val event = 1
    }
  }

}
