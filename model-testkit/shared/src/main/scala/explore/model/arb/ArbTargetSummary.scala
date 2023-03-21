// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.TargetSummary
import lucuma.core.model.Observation
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.arb.ArbTargetWithId.given
import lucuma.core.util.arb.ArbEnumerated.*
import lucuma.core.util.arb.ArbGid.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Cogen.*

trait ArbTargetSummary {
  implicit val arbTargetSummary: Arbitrary[TargetSummary] =
    Arbitrary[TargetSummary] {
      for {
        obsIds <- arbitrary[Set[Observation.Id]]
        target    <- arbitrary[TargetWithId]
      } yield TargetSummary(obsIds, target)
    }

  implicit val cogenTargetSummary: Cogen[TargetSummary] =
    Cogen[(List[Observation.Id], TargetWithId)].contramap(t => (t.obsIds.toList, t.target))
}

object ArbTargetSummary extends ArbTargetSummary
