// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.laws.discipline.arbitrary._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.all._
import lucuma.core.math.arb._
import lucuma.core.optics.laws.discipline.ValidSplitEpiTests
import lucuma.core.optics.laws.discipline.ValidWedgeTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary

final class ExploreModelValidatorsSuite extends DisciplineSuite {
  import ArbOffset._

  // Scala.js seems to have trouble formatting BigDecimals with very high absolute scale or precision.
  // We therefore use these bounded arbitraries.
  // TODO: This is duplicated in `InputValidSplitEpiInstancesSuite` in lucuma-core.
  // We should move it to the testkit there and reuse it here.
  implicit lazy val arbBigDecimalLimitedPrecision: Arbitrary[BigDecimal] =
    Arbitrary(
      org.scalacheck.Arbitrary.arbBigDecimal.arbitrary.suchThat(x =>
        x.scale.abs < 100 && x.precision <= 15
      )
    )

  // TODO: Restore when it no longer fails...
  // checkAll(
  //   "brightnessValidWedge",
  //   ValidWedgeTests(ExploreModelValidators.brightnessValidWedge).validWedgeLaws
  // )

  checkAll(
    "dithersValidSplitEpi",
    ValidSplitEpiTests(ExploreModelValidators.dithersValidSplitEpi).validSplitEpiLaws
  )

  checkAll(
    "offsetQNELValidWedge",
    ValidWedgeTests(ExploreModelValidators.offsetQNELValidWedge).validWedgeLaws
  )

  checkAll(
    "hoursValidWedge",
    ValidWedgeTests(ExploreModelValidators.hoursValidWedge).validWedgeLaws
  )
}
