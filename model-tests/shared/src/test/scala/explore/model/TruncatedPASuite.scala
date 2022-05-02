// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.EqTests
import explore.model.TruncatedPA
import explore.model.arb.ArbTruncatedPA._
import munit.DisciplineSuite

class TruncatedPASuite extends DisciplineSuite {
  checkAll("Eq[TruncatedPA]", EqTests[TruncatedPA].eqv)
}
