// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import explore.model.arb.all.given
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbTarget.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.arb.ArbTargetWithId.given
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
  checkAll("Asterism.siderealTargetsEach", TraversalTests(Asterism.siderealTargetsEach))

  test("oneTarget") {
    forAll { (id: Target.Id) =>
      checkAll("Asterism.oneTarget", IsoTests(Asterism.oneTarget(id)))
    }
  }

  test("targetOptional") {
    forAll { (id: Target.Id) =>
      given Arbitrary[Option[Asterism]] = gen.optAsterism(id)
      checkAll("Asterism.targetOptional", OptionalTests(Asterism.targetOptional(id)))
    }
  }

  test("fromTargetsListOn") {
    forAll { (id: Target.Id) =>
      given Arbitrary[Option[Asterism]] = gen.optAsterism(id)
      checkAll("Asterism.fromTargetsListOn", IsoTests(Asterism.fromTargetsListOn(Some(id))))
    }
  }

  object gen:
    // Sometimes the asterisms includes target id
    def optAsterism(id: Target.Id): Arbitrary[Option[Asterism]] =
      Arbitrary(
        Gen.option[Asterism](asterism(id).arbitrary)
      )

    def asterism(id: Target.Id): Arbitrary[Asterism] =
      Arbitrary(
        Gen.oneOf(
          arbAsterism.arbitrary,
          Gen.oneOf(
            arbAsterism.arbitrary,
            for
              ast    <- arbAsterism.arbitrary
              target <- arbitrary[Target].map(t => TargetWithId(id, t))
            yield ast.add(target)
          )
        )
      )
