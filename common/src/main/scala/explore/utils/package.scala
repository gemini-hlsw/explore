// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.Monoid
import cats.syntax.all._

package object utils {

  def abbreviate(s: String, maxLength: Int): String =
    if (s.length > maxLength) s"${s.substring(0, maxLength)}\u2026" else s

  def attemptCombine[A: Monoid, B: Monoid](a: Option[A], b: Option[B]): Option[(A, B)] =
    (a, b) match {
      case (None, None)               => none
      case (Some(someA), None)        => (someA, Monoid[B].empty).some
      case (None, Some(someB))        => (Monoid[A].empty, someB).some
      case (Some(someA), Some(someB)) => (someA, someB).some
    }
}
