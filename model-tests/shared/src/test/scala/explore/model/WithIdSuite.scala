// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.*
import explore.model.arb.all.given
import monocle.law.discipline.*
import munit.DisciplineSuite

class WithIdSuite extends DisciplineSuite:

  checkAll("Eq[TargetWithId]", EqTests[TargetWithId].eqv)
  checkAll("Eq[SiderealTargetWithId]", EqTests[SiderealTargetWithId].eqv)
  checkAll("TargetWithId.sidereal", PrismTests(TargetWithId.sidereal))
  checkAll("TargetWithId.nonsidereal", PrismTests(TargetWithId.nonsidereal))
