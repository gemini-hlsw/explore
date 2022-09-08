// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.*
// import cats.laws.discipline.arbitrary.*
// import explore.model.arb.all.*
import explore.model.arb.all.given
// import lucuma.core.model.Target
// import lucuma.core.model.arb.ArbTarget.*
// import lucuma.core.util.arb.ArbEnumerated.*
// import lucuma.core.util.arb.ArbGid.*
import monocle.law.discipline.*
import munit.DisciplineSuite

class WithIdSuite extends DisciplineSuite:

  checkAll("Eq[TargetWithId]", EqTests[TargetWithId].eqv)
  checkAll("Eq[SiderealTargetWithId]", EqTests[SiderealTargetWithId].eqv)
  checkAll("TargetWithId.sidereal", PrismTests(TargetWithId.sidereal))
  // checkAll("Asterism.fromTargetsList", IsoTests(Asterism.fromTargetsList))
  // checkAll("Asterism.targetsEach", TraversalTests(Asterism.targetsEach))
  //
  // test("targetOptional") {
  //   forAll { (id: Target.Id) =>
  //     given Arbitrary[Option[Asterism]] = gen.optAsterism(id)
  //     checkAll("Asterism.targetOptional", OptionalTests(Asterism.targetOptional(id)))
  //   }
  // }
  //
  // object gen:
  //   // Sometimes the asterisms includes target id
  //   def optAsterism(id: Target.Id): Arbitrary[Option[Asterism]] =
  //     Arbitrary(
  //       Gen.option[Asterism](
  //         Gen.oneOf(
  //           arbAsterism.arbitrary,
  //           arbTarget.arbitrary.map(t => Asterism.one(TargetWithId(id, t)))
  //         )
  //       )
  //     )
