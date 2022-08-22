// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import explore.optics.all.*
import lucuma.core.math.arb.ArbRadialVelocity
import lucuma.core.math.arb.ArbRedshift
import lucuma.core.model.arb.ArbNonNegDuration
import lucuma.core.model.arb.ArbTarget
import lucuma.core.model.implicits.*
import lucuma.core.optics.laws.discipline.SplitEpiTests
import monocle.law.discipline.IsoTests
import monocle.law.discipline.OptionalTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary.*

class ModelOpticsSuite extends DisciplineSuite {
  import ArbNonNegDuration.*
  import ArbRadialVelocity.*
  import ArbRedshift.*
  import ArbTarget.*

  checkAll("redshiftBigDecimal", IsoTests(redshiftBigDecimalIso))
  checkAll("targetRV", OptionalTests(targetRV))
  checkAll("optionNonEmptyStringIso", IsoTests(optionNonEmptyStringIso))
  checkAll("nonNegDurationSecondsSplitEpi", SplitEpiTests(nonNegDurationSecondsSplitEpi).splitEpi)
}
