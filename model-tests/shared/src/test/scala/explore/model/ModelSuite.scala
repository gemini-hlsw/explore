// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.*
import explore.model.arb.all.given
import explore.model.enums.AppTab
import lucuma.core.util.arb.ArbEnumerated.given
import munit.DisciplineSuite

class ModelSuite extends DisciplineSuite:
  checkAll("Eq[AsterismVisualOptions]", EqTests[AsterismVisualOptions].eqv)
  checkAll("Eq[SideButton]", EqTests[AppTab].eqv)
