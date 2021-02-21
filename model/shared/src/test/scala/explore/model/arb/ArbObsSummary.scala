// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import lucuma.core.model.Asterism
import lucuma.core.model.Target
import explore.model.ObsSummary
import lucuma.core.model.Observation
import lucuma.core.util.arb.ArbGid._

trait ArbObsSummary {

  implicit val obsSummaryArb = Arbitrary[ObsSummary] {
    for {
      id <- arbitrary[Observation.Id]
      nm <- arbitrary[Option[String]]
      ta <- arbitrary[Option[Either[Asterism.Id, Target.Id]]]
    } yield ObsSummary(id = id, name = nm, observationTarget = ta)
  }

  implicit val obsSummaryCogen: Cogen[ObsSummary] =
    Cogen[(Observation.Id, Option[String], Option[Either[Asterism.Id, Target.Id]])].contramap(c =>
      (c.id, c.name, c.observationTarget)
    )

}

object ArbObsSummary extends ArbObsSummary
