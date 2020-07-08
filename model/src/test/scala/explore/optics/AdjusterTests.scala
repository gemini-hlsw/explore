// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import monocle.law.discipline._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import cats.Eq

object AdjusterTests extends Laws {
  def apply[S: Arbitrary: Eq, A: Arbitrary: Eq](
    adjuster: Adjuster[S, A]
  ): RuleSet = {
    val laws: AdjusterLaws[S, A] = new AdjusterLaws(adjuster)
    new SimpleRuleSet(
      "Adjuster",
      "set idempotent"             -> forAll((s: S, a: A) => laws.setIdempotent(s, a)),
      "modify id = id"             -> forAll((s: S) => laws.modifyIdentity(s)),
      "consistent set with modify" -> forAll((s: S, a: A) => laws.consistentSetModify(s, a))
    )
  }
}
