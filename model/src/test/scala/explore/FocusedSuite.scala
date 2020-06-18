// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.implicits._
import explore.model.Focused
import explore.model.arb.all._
import gem.arb.ArbObservation._
import monocle.law.discipline.OptionalTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary._

class FocusedSuite extends DisciplineSuite {
  checkAll("obsId", OptionalTests(Focused.obsId))
  checkAll("targetId", OptionalTests(Focused.targetId))
  checkAll("targetOrObsId", OptionalTests(Focused.targetOrObsId))
}
