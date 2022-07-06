// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.TruncatedAngle
import lucuma.core.math.arb.ArbAngle._
import lucuma.core.math.Angle
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen

trait ArbTruncatedAngle {

  implicit val truncatedAngleArb: Arbitrary[TruncatedAngle] = Arbitrary[TruncatedAngle] {
    for {
      a <- arbitrary[Angle]
    } yield TruncatedAngle(a)
  }

  implicit def TruncatedAngleCogen: Cogen[TruncatedAngle] =
    Cogen[Angle].contramap(_.angle)
}

object ArbTruncatedAngle extends ArbTruncatedAngle
