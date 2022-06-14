// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.EqTests
import explore.model.TruncatedAngle
import explore.model.arb.ArbTruncatedAngle._
import munit.DisciplineSuite

class TruncatedAngleSuite extends DisciplineSuite {
  checkAll("Eq[TruncatedAngle]", EqTests[TruncatedAngle].eqv)
}
