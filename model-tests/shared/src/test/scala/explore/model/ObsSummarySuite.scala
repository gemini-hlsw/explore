// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.EqTests
import explore.model.arb.all.*
import lucuma.schemas.model.BasicConfiguration
import munit.DisciplineSuite
import org.scalacheck.Prop.*

class ObsSummarySuite extends DisciplineSuite {
  checkAll("Eq[ObsSummaryWithConstraints]", EqTests[ObsSummaryWithConstraints].eqv)
  checkAll("Eq[ObsSummaryWithTitleAndConstraints]", EqTests[ObsSummaryWithTitleAndConstraints].eqv)
  checkAll("Eq[ObsSummaryWithTitleAndConstraintsAndConf]",
           EqTests[ObsSummaryWithTitleConstraintsAndConf].eqv
  )
  checkAll("Eq[ObsSummaryWithTitleAndConf]", EqTests[ObsSummaryWithTitleAndConf].eqv)

  property("ObsWithConf should include grating and fpu shortName") {
    forAll { (c: ObsSummaryWithTitleAndConf) =>
      c.configuration match
        case None                                              =>
          assertEquals(c.conf, "-")
        case Some(value: BasicConfiguration.GmosNorthLongSlit) =>
          assert(c.conf.contains(value.grating.shortName))
          assert(c.conf.contains(value.fpu.shortName))
        case Some(value: BasicConfiguration.GmosSouthLongSlit) =>
          assert(c.conf.contains(value.grating.shortName))
          assert(c.conf.contains(value.fpu.shortName))
    }
  }
}
