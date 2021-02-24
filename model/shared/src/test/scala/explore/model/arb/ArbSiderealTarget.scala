// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import java.util.UUID

import explore.model.SiderealTarget
import explore.model.arb.CogenUUID._
import lucuma.core.model.SiderealTracking
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string._

trait ArbSiderealTarget {
  import lucuma.core.model.arb.ArbSiderealTracking._

  implicit val targetArb = Arbitrary[SiderealTarget] {
    for {
      i <- arbitrary[UUID]
      n <- arbitrary[NonEmptyString]
      p <- arbitrary[SiderealTracking]
    } yield SiderealTarget(i, n, p)
  }

  implicit val siderealTargetCogen: Cogen[SiderealTarget] =
    Cogen[(UUID, String, SiderealTracking)].contramap(c => (c.id, c.name.value, c.track))
}

object ArbSiderealTarget extends ArbSiderealTarget
