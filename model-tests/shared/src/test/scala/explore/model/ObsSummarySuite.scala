// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.EqTests
import explore.model.arb.ArbObsSummary
import munit.DisciplineSuite
import org.scalacheck.Prop.*

class ObsSummarySuite extends DisciplineSuite:
  import ArbObsSummary.given

  checkAll("Eq[ObsSummary]", EqTests[ObsSummary].eqv)
