// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import monocle.law.discipline.LensTests
import monocle.law.discipline.IsoTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary._
import explore.common.TargetObsQueries
import explore.common.arb.ArbTargetObsQueries
import explore.model.arb.ArbPointingId

class TargetObsQueriesSuite extends DisciplineSuite {
  import ArbTargetObsQueries._
  import ArbPointingId._

  checkAll("targetsObsQueryPointingId", IsoTests(TargetObsQueries.targetsObsQueryPointingId))
  checkAll("targetsObsQueryObsPointingId", LensTests(TargetObsQueries.targetsObsQueryObsPointingId))
}
