// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.TargetSummary
import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import lucuma.core.util.arb.ArbGid._
import eu.timepit.refined.scalacheck._
import lucuma.core.model.Observation
import lucuma.core.model.Target

trait ArbTargetSummary {
  implicit val arbTargetSummary: Arbitrary[TargetSummary] =
    Arbitrary[TargetSummary] {
      for {
        obsIds <- arbitrary[Set[Observation.Id]]
        tid    <- arbitrary[Target.Id]
      } yield TargetSummary(obsIds, tid)
    }

  implicit val cogenTargetSummary: Cogen[TargetSummary] =
    Cogen[(List[Observation.Id], Target.Id)].contramap(t => (t.obsIds.toList, t.targetId))
}

object ArbTargetSummary extends ArbTargetSummary
