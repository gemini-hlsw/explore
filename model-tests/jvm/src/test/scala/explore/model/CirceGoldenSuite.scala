// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.arb.all._
import io.circe.testing.golden.GoldenCodecTests
import io.circe.testing.ArbitraryInstances
import munit.DisciplineSuite

class CirceGoldenSuite extends DisciplineSuite with ArbitraryInstances {
  checkAll("GoldenCodec[ConstraintSetModel]", GoldenCodecTests[ConstraintSetModel](5).goldenCodec)
  checkAll("GoldenCodec[ConstraintsSummary]", GoldenCodecTests[ConstraintsSummary](5).goldenCodec)
}
