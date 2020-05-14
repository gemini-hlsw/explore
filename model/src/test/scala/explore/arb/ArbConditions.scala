// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import explore.model.enum._
import explore.model.Conditions
import gem.arb.ArbEnumerated._

trait ArbConditions {

  implicit val conArb = Arbitrary[Conditions] {
    for {
      cc <- arbitrary[CloudCover]
      iq <- arbitrary[ImageQuality]
      sb <- arbitrary[SkyBackground]
      wv <- arbitrary[WaterVapor]
    } yield Conditions(cc, iq, sb, wv)
  }

  implicit val conCogen: Cogen[Conditions] =
    Cogen[(CloudCover, ImageQuality, SkyBackground, WaterVapor)].contramap(c =>
      (c.cc, c.iq, c.sb, c.wv)
    )

}

object ArbConditions extends ArbConditions
