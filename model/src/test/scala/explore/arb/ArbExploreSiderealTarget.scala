// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import gem.arb.ArbEnumerated._
import explore.model.SiderealTarget
import explore.model.ExploreSiderealTarget

trait ArbExploreSiderealTarget {
  import explore.model.arb.ArbSiderealTarget._

  implicit val exploreSiderealTargetArb = Arbitrary[ExploreSiderealTarget] {
    for {
      s <- arbitrary[String]
      t <- arbitrary[Option[SiderealTarget]]
    } yield ExploreSiderealTarget(s, t)
  }

  implicit val exploreSiderealTargetCogen: Cogen[ExploreSiderealTarget] =
    Cogen[(String, Option[SiderealTarget])].contramap(c => (c.searchTerm, c.target))

}

object ArbExploreSiderealTarget extends ArbExploreSiderealTarget
