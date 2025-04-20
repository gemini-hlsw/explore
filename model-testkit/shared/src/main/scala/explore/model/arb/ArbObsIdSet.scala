// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.data.NonEmptySet
import cats.laws.discipline.arbitrary.*
import explore.model.ObsIdSet
import explore.model.Observation
import lucuma.core.util.arb.ArbGid.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbObsIdSet {

  given Arbitrary[ObsIdSet] = Arbitrary {
    arbitrary[NonEmptySet[Observation.Id]].map(ObsIdSet.apply)
  }

  given Cogen[ObsIdSet] =
    Cogen[NonEmptySet[Observation.Id]].contramap(_.idSet)
}

object ArbObsIdSet extends ArbObsIdSet
