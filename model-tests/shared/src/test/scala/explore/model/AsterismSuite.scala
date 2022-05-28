// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline._
import cats.laws.discipline.arbitrary._
import explore.model.arb.all._
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbTarget._
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.util.arb.ArbGid._
import monocle.law.discipline._
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Prop._

class AsterismSuite extends DisciplineSuite {
  checkAll("Eq[AsterismSuite]", EqTests[Asterism].eqv)
  checkAll("Asterism.isoTargets", IsoTests(Asterism.isoTargets))
  checkAll("Asterism.fromTargetsList", IsoTests(Asterism.fromTargetsList))

  test("targetOptional") {
    forAll { (id: Target.Id) =>
      // Sometimes the asterisms includes target id
      implicit val optAsterism: Arbitrary[Option[Asterism]] =
        Arbitrary(
          Gen.option[Asterism](
            Gen.oneOf(arbAsterism.arbitrary,
                      Gen.const(Asterism.of(TargetWithId(id, arbTarget.arbitrary.sample.get)))
            )
          )
        )

      checkAll("Asterism.targetOptional", OptionalTests(Asterism.targetOptional(id)))
    }
  }

}
