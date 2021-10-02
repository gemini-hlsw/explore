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

trait ArbConstraintsSummary {
  def buildConstraintsSummaryArb[A <: ConstraintsSummary](
    build: (
      ImageQuality,
      CloudExtinction,
      SkyBackground,
      WaterVapor
    ) => A
  ) = Arbitrary[A] {
    for {
      iq <- arbitrary[ImageQuality]
      ce <- arbitrary[CloudExtinction]
      sb <- arbitrary[SkyBackground]
      wv <- arbitrary[WaterVapor]
    } yield build(iq, ce, sb, wv)
  }

  implicit val constraintsSummaryArb = buildConstraintsSummaryArb((iq, ce, sb, wv) =>
    new ConstraintsSummary {
      val imageQuality    = iq
      val cloudExtinction = ce
      val skyBackground   = sb
      val waterVapor      = wv
    }
  )

  implicit val constraintsSummaryCogen: Cogen[ConstraintsSummary] =
    Cogen[(ImageQuality, CloudExtinction, SkyBackground, WaterVapor)]
      .contramap(cs => (cs.imageQuality, cs.cloudExtinction, cs.skyBackground, cs.waterVapor))
}

object ArbConstraintsSummary extends ArbConstraintsSummary
