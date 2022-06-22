// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline._
import explore.model.arb.all._
import explore.model.enums.AppTab
import lucuma.core.util.arb.ArbEnumerated._
import munit.DisciplineSuite

class ModelSuite extends DisciplineSuite {
  checkAll("Eq[TargetVisualOptions]", EqTests[TargetVisualOptions].eqv)
  checkAll("Eq[SideButton]", EqTests[AppTab].eqv)
  checkAll("Eq[UserVault]", EqTests[UserVault].eqv)

}
