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
import lucuma.core.util.arb.ArbGid._
import lucuma.core.model.ConstraintSet
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.scalacheck.string._

trait ArbConstraintsSummary {
  def buildConstraintsSummaryArb[A <: ConstraintsSummary](
    build: (
      ConstraintSet.Id,
      NonEmptyString,
      ImageQuality,
      CloudExtinction,
      SkyBackground,
      WaterVapor
    ) => A
  ) = Arbitrary[A] {
    for {
      id <- arbitrary[ConstraintSet.Id]
      nm <- arbitrary[NonEmptyString]
      iq <- arbitrary[ImageQuality]
      ce <- arbitrary[CloudExtinction]
      sb <- arbitrary[SkyBackground]
      wv <- arbitrary[WaterVapor]
    } yield build(id, nm, iq, ce, sb, wv)
  }

  implicit val constraintsSummaryArb = buildConstraintsSummaryArb((i, nm, iq, ce, sb, wv) =>
    new ConstraintsSummary {
      val id              = i
      val name            = nm
      val imageQuality    = iq
      val cloudExtinction = ce
      val skyBackground   = sb
      val waterVapor      = wv
    }
  )

  implicit val constraintsSummaryCogen: Cogen[ConstraintsSummary] =
    Cogen[(ConstraintSet.Id, ImageQuality, CloudExtinction, SkyBackground, WaterVapor)]
      .contramap(cs =>
        (cs.id, cs.imageQuality, cs.cloudExtinction, cs.skyBackground, cs.waterVapor)
      )
}

object ArbConstraintsSummary extends ArbConstraintsSummary
