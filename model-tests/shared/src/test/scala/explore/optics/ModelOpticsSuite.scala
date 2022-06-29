// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import lucuma.core.math.arb.ArbRadialVelocity
import lucuma.core.math.arb.ArbRedshift
import lucuma.core.model.arb.ArbTarget
import explore.optics.all._
import monocle.law.discipline.IsoTests
import monocle.law.discipline.OptionalTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary._

class ModelOpticsSuite extends DisciplineSuite {
  import ArbRadialVelocity._
  import ArbRedshift._
  import ArbTarget._

  checkAll("redshiftBigDecimal", IsoTests(redshiftBigDecimalIso))
  checkAll("targetRV", OptionalTests(targetRV))
}
