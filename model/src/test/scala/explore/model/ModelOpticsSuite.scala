// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.arb.all._
import explore.optics._
import lucuma.core.math.arb.ArbDeclination
import lucuma.core.math.arb.ArbRightAscension
import lucuma.core.model.arb.ArbSiderealTracking
import monocle.law.discipline.LensTests
import monocle.law.discipline.IsoTests
import munit.DisciplineSuite
import lucuma.core.math.arb.ArbRadialVelocity
import lucuma.core.math.arb.ArbApparentRadialVelocity
import lucuma.core.math.arb.ArbRedshift
import org.scalacheck.Arbitrary._

class ModelOpticsSuite
    extends DisciplineSuite
    with ArbDeclination
    with ArbRightAscension
    with ArbRadialVelocity
    with ArbRedshift
    with ArbApparentRadialVelocity
    with ArbSiderealTracking {

  checkAll("properMotionRA", LensTests(ModelOptics.properMotionRA))
  checkAll("properMotionDec", LensTests(ModelOptics.properMotionDec))
  checkAll("targetRA", LensTests(ModelOptics.targetRA))
  checkAll("targetDec", LensTests(ModelOptics.targetDec))
  checkAll("redshiftBigDecimal", IsoTests(redshiftBigDecimalISO))
}
