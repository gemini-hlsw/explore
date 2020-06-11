// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.implicits._
import explore.model.arb.all._
import gsp.math.arb.ArbDeclination
import gsp.math.arb.ArbProperMotion
import gsp.math.arb.ArbRightAscension
import monocle.law.discipline.LensTests
import munit.DisciplineSuite
import munit.DisciplineSuite

class ModelOpticsSuite
    extends DisciplineSuite
    with ArbDeclination
    with ArbRightAscension
    with ArbProperMotion {
  checkAll("properMotionRA", LensTests(ModelOptics.properMotionRA))
  checkAll("properMotionDec", LensTests(ModelOptics.properMotionDec))
  checkAll("targetRA", LensTests(ModelOptics.targetRA))
  checkAll("targetDec", LensTests(ModelOptics.targetDec))
  checkAll("targetPropsL", LensTests(ModelOptics.targetPropsL))
}
