// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import java.util.UUID

import explore.model.SiderealTarget
import gsp.math.ProperMotion
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._

trait ArbSiderealTarget {
  import gsp.math.arb.ArbProperMotion._

  implicit val targetArb = Arbitrary[SiderealTarget] {
    for {
      i <- arbitrary[UUID]
      n <- arbitrary[String]
      p <- arbitrary[ProperMotion]
    } yield SiderealTarget(i, n, p)
  }

  implicit val siderealTargetCogen: Cogen[SiderealTarget] =
    Cogen[(String, ProperMotion)].contramap(c => (c.name, c.track))
}

object ArbSiderealTarget extends ArbSiderealTarget
