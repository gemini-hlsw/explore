// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.laws.discipline.arbitrary.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.arb.*
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.arb.ArbAngle.*
import lucuma.core.math.arb.ArbBrightnessValue.given
import lucuma.core.math.arb.ArbOffset.*
import lucuma.core.math.arb.ArbParallax.*
import lucuma.core.math.arb.ArbProperMotion.given
import lucuma.core.math.arb.ArbWavelengthDither.given
import lucuma.core.optics.laws.discipline.ValidSplitEpiTests
import lucuma.core.optics.laws.discipline.ValidWedgeTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen

final class ExploreModelValidatorsSuite extends DisciplineSuite:
  checkAll(
    "brightnessValidWedge",
    ValidWedgeTests(ExploreModelValidators.brightnessValidWedge).validWedgeLaws
  )

  checkAll(
    "ditherValidWedge",
    ValidWedgeTests(ExploreModelValidators.ditherValidWedge).validWedgeLaws
  )

  checkAll(
    "offsetQNELValidWedge",
    ValidWedgeTests(ExploreModelValidators.offsetQNELValidWedge).validWedgeLaws
  )

  checkAll(
    "hoursValidWedge",
    ValidWedgeTests(ExploreModelValidators.hoursValidWedge).validWedgeLaws
  )

  private val perturbations: List[String => Gen[String]] =
    List(
      _ => arbitrary[String],             // swap for a random string
      s => Gen.const(s.replace("2", "0")) // create a leading zero, maybe (ok)
    )

  val milliArcSecondsGen: Gen[String] =
    arbitrary[Angle]
      .map(Angle.signedDecimalMilliarcseconds.get(_).toString)
      .flatMapOneOf(Gen.const[String], perturbations: _*)

  checkAll(
    "compactDecimalStringValidWedge",
    ValidWedgeTests(ExploreModelValidators.compactDecimalStringValidWedge).validWedgeLaws
  )

  checkAll(
    "pxValidWedge",
    ValidWedgeTests(ExploreModelValidators.pxValidWedge)
      .validWedgeLawsWith(milliArcSecondsGen)
  )

  checkAll(
    "pmRAValidWedge",
    ValidWedgeTests(ExploreModelValidators.pmRAValidWedge)
      .validWedgeLawsWith(milliArcSecondsGen)
  )

  checkAll(
    "pmDecValidWedge",
    ValidWedgeTests(ExploreModelValidators.pmDecValidWedge)
      .validWedgeLawsWith(milliArcSecondsGen)
  )
