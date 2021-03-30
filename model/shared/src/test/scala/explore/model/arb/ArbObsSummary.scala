// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import explore.model.PointingId
import explore.model.ObsSummary
import explore.model.ConstraintsSummary
import lucuma.core.model.Observation
import lucuma.core.util.arb.ArbGid._
// these need to be at the end to avoid diverging implicit expansion problems
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string._

trait ArbObsSummary {
  import ArbConstraintsSummary._

  implicit val obsSummaryArb = Arbitrary[ObsSummary] {
    for {
      id  <- arbitrary[Observation.Id]
      nm  <- arbitrary[NonEmptyString]
      aim <- arbitrary[Option[PointingId]]
      cs  <- arbitrary[Option[ConstraintsSummary]]
    } yield ObsSummary(id = id, name = nm, pointingId = aim, constraints = cs)
  }

  implicit val obsSummaryCogen: Cogen[ObsSummary] =
    Cogen[(Observation.Id, String, Option[PointingId], Option[ConstraintsSummary])]
      .contramap(c => (c.id, c.name.value, c.pointingId, c.constraints))

}

object ArbObsSummary extends ArbObsSummary
