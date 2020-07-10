// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.Constraints
import explore.model.arb.CogenUUID._
import explore.model.enum._
import gem.arb.ArbEnumerated._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._

trait ArbConstraints {

  implicit val conditionsArb = Arbitrary[Constraints] {
    for {
      id <- arbitrary[Constraints.Id]
      nm <- arbitrary[String]
      cc <- arbitrary[CloudCover]
      iq <- arbitrary[ImageQuality]
      sb <- arbitrary[SkyBackground]
      wv <- arbitrary[WaterVapor]
    } yield Constraints(id, nm, cc, iq, sb, wv)
  }

  implicit val conditionsCogen: Cogen[Constraints] =
    Cogen[(Constraints.Id, String, CloudCover, ImageQuality, SkyBackground, WaterVapor)].contramap(
      c => (c.id, c.name, c.cc, c.iq, c.sb, c.wv)
    )

}

object ArbConstraints extends ArbConstraints
