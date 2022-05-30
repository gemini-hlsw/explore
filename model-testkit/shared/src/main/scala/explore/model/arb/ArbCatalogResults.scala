// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.CatalogResults
import lucuma.core.model.arb._
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.scalacheck.string._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import lucuma.ags.GuideStarCandidate
import lucuma.core.model.SiderealTracking

trait ArbCatalogResults {
  import ArbSiderealTracking._

  implicit val guideStarCandidateArb = Arbitrary[GuideStarCandidate] {
    for {
      name        <- arbitrary[NonEmptyString]
      tracking    <- arbitrary[SiderealTracking]
      gBrightness <- arbitrary[Option[BigDecimal]]
    } yield GuideStarCandidate(name, tracking, gBrightness)
  }

  implicit def guideStarCandidateCogen: Cogen[GuideStarCandidate] =
    Cogen[(String, SiderealTracking, Option[BigDecimal])].contramap(m =>
      (m.name.value, m.tracking, m.gBrightness)
    )

  implicit val catalogResultsArb = Arbitrary[CatalogResults] {
    for {
      candidates <- arbitrary[List[GuideStarCandidate]]
    } yield CatalogResults(candidates)
  }

  implicit def catalogResultsCogen: Cogen[CatalogResults] =
    Cogen[List[GuideStarCandidate]].contramap(_.candidates)

}

object ArbCatalogResults extends ArbCatalogResults
