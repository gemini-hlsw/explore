// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.events.CatalogResults
import lucuma.ags.arb.ArbGuideStarCandidate._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import lucuma.ags.GuideStarCandidate

trait ArbCatalogResults {

  implicit val catalogResultsArb: Arbitrary[CatalogResults] = Arbitrary[CatalogResults] {
    for {
      candidates <- arbitrary[List[GuideStarCandidate]]
    } yield CatalogResults(candidates)
  }

  implicit def catalogResultsCogen: Cogen[CatalogResults] =
    Cogen[List[GuideStarCandidate]].contramap(_.candidates)

}

object ArbCatalogResults extends ArbCatalogResults
