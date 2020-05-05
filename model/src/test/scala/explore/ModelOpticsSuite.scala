// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import org.typelevel.discipline.munit.Discipline
import monocle.law.discipline.LensTests
import gsp.math.arb.ArbRightAscension
import gsp.math.arb.ArbProperMotion
import gsp.math.arb.ArbDeclination
import explore.model.arb.ArbSiderealTarget

class ModelOpticsSuite
    extends munit.FunSuite
    with Discipline
    with ArbSiderealTarget
    with ArbDeclination
    with ArbRightAscension
    with ArbProperMotion {
  checkAll("properMotionRA", LensTests(ModelOptics.properMotionRA))
  checkAll("properMotionDec", LensTests(ModelOptics.properMotionDec))
  checkAll("targetRA", LensTests(ModelOptics.targetRA))
  checkAll("targetDec", LensTests(ModelOptics.targetDec))
}
