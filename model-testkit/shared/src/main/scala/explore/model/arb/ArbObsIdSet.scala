// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.data.NonEmptySet
import cats.laws.discipline.arbitrary._
import explore.model.ObsIdSet
import lucuma.core.model.Observation
import lucuma.core.util.arb.ArbGid._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen

trait ArbObsIdSet {

  implicit val arbObsIdSet: Arbitrary[ObsIdSet] = Arbitrary {
    arbitrary[NonEmptySet[Observation.Id]].map(ObsIdSet.apply)
  }

  implicit val cogenObsIdSet: Cogen[ObsIdSet] =
    Cogen[NonEmptySet[Observation.Id]].contramap(_.idSet)
}

object ArbObsIdSet extends ArbObsIdSet
