// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.EqTests
import explore.model.RootModel
import explore.model.arb.ArbRootModel
import munit.DisciplineSuite

class RootModelSuite extends DisciplineSuite {
  import ArbRootModel._

  checkAll("Eq[RootModel]", EqTests[RootModel].eqv)
}
