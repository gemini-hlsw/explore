// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.TruncatedPA
import lucuma.core.math.arb.ArbAngle._
import lucuma.core.math.Angle
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen

trait ArbTruncatedPA {

  implicit val truncatedPAArb = Arbitrary[TruncatedPA] {
    for {
      a <- arbitrary[Angle]
    } yield TruncatedPA(a)
  }

  implicit def truncatedPACogen: Cogen[TruncatedPA] =
    Cogen[Angle].contramap(_.angle)

}

object ArbTruncatedPA extends ArbTruncatedPA
