// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.EqTests
import explore.model.arb.all._
import io.circe.testing.ArbitraryInstances
import munit.DisciplineSuite

class PointingIdSuite extends DisciplineSuite with ArbitraryInstances {
  checkAll("Eq[PointingId]", EqTests[PointingId].eqv)
}
