// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline._
import explore.model.arb.all._
import explore.model.enum.AppTab
import munit.DisciplineSuite

class ModelSuite extends DisciplineSuite {
  checkAll("Eq[Conditions]", EqTests[Conditions].eqv)
  checkAll("Eq[SiderealTarget]", EqTests[SiderealTarget].eqv)
  checkAll("Eq[ExploreSiderealTarget]", EqTests[ExploreSiderealTarget].eqv)
  checkAll("Eq[SideButton]", EqTests[AppTab].eqv)
}
