// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.arb.ArbObsIdSet.given
import monocle.law.discipline.*
import munit.DisciplineSuite

class ObsIdSetSuite extends DisciplineSuite {
  checkAll("ObsIdSet.fromString", PrismTests(ObsIdSet.fromString))
}
