// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

// import explore.events.CatalogResults
// import lucuma.core.model.arb._
// import org.scalacheck.Arbitrary
// import org.scalacheck.Arbitrary.arbitrary
// import org.scalacheck.Cogen
// import lucuma.core.model.SiderealTracking
// import lucuma.ags.GuideStarCandidate
//
// trait ArbCatalogResults {
//   import ArbSiderealTracking._
//
//   implicit val guideStarCandidateArb: Arbitrary[GuideStarCandidate] =
//     Arbitrary[GuideStarCandidate] {
//       for {
//         name        <- arbitrary[Long]
//         tracking    <- arbitrary[SiderealTracking]
//         gBrightness <- arbitrary[Option[BigDecimal]]
//       } yield GuideStarCandidate(name, tracking, gBrightness)
//     }
//
//   implicit def guideStarCandidateCogen: Cogen[GuideStarCandidate] =
//     Cogen[(Long, SiderealTracking, Option[BigDecimal])].contramap(m =>
//       (m.id, m.tracking, m.gBrightness)
//     )
//
//   implicit val catalogResultsArb: Arbitrary[CatalogResults] = Arbitrary[CatalogResults] {
//     for {
//       candidates <- arbitrary[List[GuideStarCandidate]]
//     } yield CatalogResults(candidates)
//   }
//
//   implicit def catalogResultsCogen: Cogen[CatalogResults] =
//     Cogen[List[GuideStarCandidate]].contramap(_.candidates)
//
// }
//
// object ArbCatalogResults extends ArbCatalogResults
