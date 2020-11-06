// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Monoid
import cats.syntax.all._
import lucuma.core.math.ProperVelocity

object utils {
  def attemptCombine[A: Monoid, B: Monoid](a: Option[A], b: Option[B]): Option[(A, B)] =
    (a, b) match {
      case (None, None)               => none
      case (Some(someA), None)        => (someA, Monoid[B].empty).some
      case (None, Some(someB))        => (Monoid[A].empty, someB).some
      case (Some(someA), Some(someB)) => (someA, someB).some
    }

  def buildProperVelocity(
    ra:  Option[ProperVelocity.RA],
    dec: Option[ProperVelocity.Dec]
  ): Option[ProperVelocity] =
    attemptCombine(ra, dec)
      .map((ProperVelocity.apply _).tupled)
}
