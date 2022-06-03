// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.EqTests
import explore.model.arb.ArbCatalogResults._
import lucuma.ags.GuideStarCandidate
import lucuma.core.model.arb.ArbTarget._
import lucuma.core.optics.laws.discipline.SplitEpiTests
import munit.DisciplineSuite

class CatalogResultsSuite extends DisciplineSuite {
  checkAll("Eq[GuideStarCandidate]", EqTests[GuideStarCandidate].eqv)
  checkAll("Eq[CatalogResults]", EqTests[CatalogResults].eqv)

  // optics
  checkAll("GuideStarCandidate.siderealTarget",
           SplitEpiTests(GuideStarCandidate.siderealTarget).splitEpi
  )
}
