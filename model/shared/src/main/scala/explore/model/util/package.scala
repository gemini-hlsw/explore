// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Monoid
import cats.syntax.all._
import lucuma.core.math.ProperMotion

package object util {
  def attemptCombine[A: Monoid, B: Monoid](a: Option[A], b: Option[B]): Option[(A, B)] =
    (a, b) match {
      case (None, None)               => none
      case (Some(someA), None)        => (someA, Monoid[B].empty).some
      case (None, Some(someB))        => (Monoid[A].empty, someB).some
      case (Some(someA), Some(someB)) => (someA, someB).some
    }

  def buildProperMotion(
    ra:  Option[ProperMotion.RA],
    dec: Option[ProperMotion.Dec]
  ): Option[ProperMotion] =
    attemptCombine(ra, dec)
      .map((ProperMotion.apply _).tupled)

  def unsafeOptionFnUnlift[A](fn: Option[A] => Option[A]): A => A =
    a => fn(a.some).get
}
