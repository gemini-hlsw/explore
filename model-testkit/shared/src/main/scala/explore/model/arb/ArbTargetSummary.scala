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
import lucuma.core.model.Target
import eu.timepit.refined.scalacheck._
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string._

trait ArbTargetSummary {
  implicit val arbTargetSummary =
    Arbitrary[TargetSummary] {
      for {
        id   <- arbitrary[Target.Id]
        name <- arbitrary[NonEmptyString]
      } yield TargetSummary(id, name)
    }

  implicit val cogenTargetSummary: Cogen[TargetSummary] =
    Cogen[(Target.Id, NonEmptyString)].contramap(t => (t.id, t.name))
}

object ArbTargetSummary extends ArbTargetSummary
