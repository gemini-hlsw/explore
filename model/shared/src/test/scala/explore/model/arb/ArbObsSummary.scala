// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import explore.model.AimId
import explore.model.ObsSummary
import lucuma.core.model.Observation
import lucuma.core.util.arb.ArbGid._

trait ArbObsSummary {

  implicit val obsSummaryArb = Arbitrary[ObsSummary] {
    for {
      id  <- arbitrary[Observation.Id]
      nm  <- arbitrary[Option[String]]
      aim <- arbitrary[Option[AimId]]
    } yield ObsSummary(id = id, name = nm, aimId = aim)
  }

  implicit val obsSummaryCogen: Cogen[ObsSummary] =
    Cogen[(Observation.Id, Option[String], Option[AimId])].contramap(c => (c.id, c.name, c.aimId))

}

object ArbObsSummary extends ArbObsSummary
