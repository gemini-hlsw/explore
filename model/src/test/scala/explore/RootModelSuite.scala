// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.implicits._
import cats.kernel.laws.discipline.EqTests
import explore.model.RootModel
import explore.model.arb.all._
import gem.arb.ArbObservation._
import monocle.law.discipline.OptionalTests
import munit.DisciplineSuite

class RootModelSuite extends DisciplineSuite {
  checkAll("Eq[RootModel]", EqTests[RootModel].eqv)
  checkAll("focusedOpt", OptionalTests(RootModel.focusedOpt))
  checkAll("focusedObsId", OptionalTests(RootModel.focusedObsId))
  checkAll("focusedTargetId", OptionalTests(RootModel.focusedTargetId))
  checkAll("focusedTargetOrObsId", OptionalTests(RootModel.focusedTargetOrObsId))
}
