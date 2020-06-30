// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.implicits._
import cats.kernel.laws.discipline.EqTests
import explore.model.RootModel
import explore.model.arb.all._
import munit.DisciplineSuite

class RootModelSuite extends DisciplineSuite {
  checkAll("Eq[RootModel]", EqTests[RootModel].eqv)
}
