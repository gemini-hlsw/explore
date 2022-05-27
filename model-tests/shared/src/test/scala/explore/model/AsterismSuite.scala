// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline._
import explore.model.arb.all._
import lucuma.core.util.arb.ArbEnumerated._
import monocle.law.discipline._
import cats.laws.discipline.arbitrary._
import munit.DisciplineSuite

class AsterismSuite extends DisciplineSuite {
  checkAll("Eq[AsterismSuite]", EqTests[Asterism].eqv)
  checkAll("Asterism.isoTargets", IsoTests(Asterism.isoTargets))
  checkAll("Asterism.fromTargetsList", IsoTests(Asterism.fromTargetsList))

}
