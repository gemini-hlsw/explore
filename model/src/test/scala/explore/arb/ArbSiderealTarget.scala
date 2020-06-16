// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.SiderealTarget
import gem.arb.ArbEnumerated._
import gsp.math.ProperMotion
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._

trait ArbSiderealTarget {
  import gsp.math.arb.ArbProperMotion._

  implicit val targetArb = Arbitrary[SiderealTarget] {
    for {
      n <- arbitrary[String]
      p <- arbitrary[ProperMotion]
    } yield SiderealTarget(n, p)
  }

  implicit val siderealTargetCogen: Cogen[SiderealTarget] =
    Cogen[(String, ProperMotion)].contramap(c => (c.name, c.track))

  implicit val siderealTargetId: Arbitrary[SiderealTarget.Id] =
    Arbitrary(
      arbitrary[String].map(SiderealTarget.Id.apply)
    )

  implicit val cogSiderealTargetId: Cogen[SiderealTarget.Id] =
    Cogen[String].contramap(_.id)
}

object ArbSiderealTarget extends ArbSiderealTarget
