// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.ExploreSiderealTarget
import explore.model.SiderealTarget
import explore.model.TargetVisualOptions
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._

trait ArbExploreSiderealTarget {
  import explore.model.arb.ArbSiderealTarget._
  import explore.model.arb.ArbTargetVisualOptions._

  implicit val exploreSiderealTargetArb = Arbitrary[ExploreSiderealTarget] {
    for {
      s <- arbitrary[String]
      t <- arbitrary[Option[SiderealTarget]]
      o <- arbitrary[TargetVisualOptions]
    } yield ExploreSiderealTarget(s, t, o)
  }

  implicit val exploreSiderealTargetCogen: Cogen[ExploreSiderealTarget] =
    Cogen[(String, Option[SiderealTarget])].contramap(c => (c.searchTerm, c.target))

}

object ArbExploreSiderealTarget extends ArbExploreSiderealTarget
