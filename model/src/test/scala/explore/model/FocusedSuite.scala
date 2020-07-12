// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.EqTests
import explore.model.Focused
import explore.model.arb.all._
import munit.DisciplineSuite

class FocusedSuite extends DisciplineSuite {
  checkAll("Eq[Focused]", EqTests[Focused].eqv)
}
