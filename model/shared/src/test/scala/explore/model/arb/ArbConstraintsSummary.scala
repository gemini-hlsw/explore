// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import explore.model.ConstraintsSummary
import lucuma.core.enum._
import lucuma.core.model.ConstraintSet
import lucuma.core.util.arb.ArbGid._
// these need to be at the end to avoid diverging implicit expansion problems
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string._

trait ArbConstraintsSummary {
  implicit val constraintsSummaryArb = Arbitrary[ConstraintsSummary] {
    for {
      id   <- arbitrary[ConstraintSet.Id]
      name <- arbitrary[NonEmptyString]
      iq   <- arbitrary[ImageQuality]
      ce   <- arbitrary[CloudExtinction]
      sb   <- arbitrary[SkyBackground]
      wv   <- arbitrary[WaterVapor]
    } yield ConstraintsSummary(id = id,
                               name = name,
                               imageQuality = iq,
                               cloudExtinction = ce,
                               skyBackground = sb,
                               waterVapor = wv
    )
  }

  implicit val constraintsSummaryCogen: Cogen[ConstraintsSummary] =
    Cogen[(ConstraintSet.Id, String, ImageQuality, CloudExtinction, SkyBackground, WaterVapor)]
      .contramap(cs =>
        (cs.id, cs.name.value, cs.imageQuality, cs.cloudExtinction, cs.skyBackground, cs.waterVapor)
      )
}

object ArbConstraintsSummary extends ArbConstraintsSummary
