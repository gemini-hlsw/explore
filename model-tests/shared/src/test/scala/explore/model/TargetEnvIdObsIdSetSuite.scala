// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.TargetEnvIdObsIdSet
import explore.model.arb.ArbTargetEnvIdObsIdSet._
import lucuma.core.optics.laws.discipline.FormatTests
import munit.DisciplineSuite

class TargetEnvIdObsIdSetSuite extends DisciplineSuite {
  checkAll("TargetEnvIdObsIdSet.format",
           FormatTests(TargetEnvIdObsIdSet.format).formatWith(stringsOftenParsable)
  )
}
