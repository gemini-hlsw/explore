// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import explore.model.arb.all.*
import explore.model.arb.all.given
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbTarget.*
import lucuma.core.util.arb.ArbEnumerated.*
import lucuma.core.util.arb.ArbGid.*
import monocle.law.discipline.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import org.scalacheck.Test

class AsterismSuite extends DisciplineSuite:
  override val scalaCheckTestParameters = Test.Parameters.default.withMaxSize(10)

  checkAll("Eq[AsterismSuite]", EqTests[Asterism].eqv)
  checkAll("Asterism.isoTargets", IsoTests(Asterism.isoTargets))
  checkAll("Asterism.fromTargetsList", IsoTests(Asterism.fromTargetsList))
  checkAll("Asterism.targetsEach", TraversalTests(Asterism.targetsEach))

  test("targetOptional") {
    forAll { (id: Target.Id) =>
      given Arbitrary[Option[Asterism]] = gen.optAsterism(id)
      checkAll("Asterism.targetOptional", OptionalTests(Asterism.targetOptional(id)))
    }
  }

  object gen:
    // Sometimes the asterisms includes target id
    def optAsterism(id: Target.Id): Arbitrary[Option[Asterism]] =
      Arbitrary(
        Gen.option[Asterism](
          Gen.oneOf(
            arbAsterism.arbitrary,
            arbTarget.arbitrary.map(t => Asterism.one(TargetWithId(id, t)))
          )
        )
      )
