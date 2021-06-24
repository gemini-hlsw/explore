// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import coulomb.accepted.Percent
import coulomb.cats.implicits._
import explore.optics._
import lucuma.core.math.arb.ArbApparentRadialVelocity
import lucuma.core.math.arb.ArbDeclination
import lucuma.core.math.arb.ArbRadialVelocity
import lucuma.core.math.arb.ArbRedshift
import lucuma.core.math.arb.ArbRightAscension
import lucuma.core.model.arb.ArbSiderealTracking
import monocle.law.discipline.IsoTests
import monocle.law.discipline.LensTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary._

class ModelOpticsSuite
    extends DisciplineSuite
    with ArbDeclination
    with ArbRightAscension
    with ArbRadialVelocity
    with ArbRedshift
    with ArbApparentRadialVelocity
    with ArbSiderealTracking {
  import coulomb.scalacheck.ArbQuantity._ // This import has to be last.

  checkAll("properMotionRA", LensTests(ModelOptics.properMotionRA))
  checkAll("properMotionDec", LensTests(ModelOptics.properMotionDec))
  checkAll("redshiftBigDecimal", IsoTests(redshiftBigDecimalISO))

  checkAll("coulombIso", IsoTests(coulombIso[Int, Percent]))
}
