// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import explore.model.arb.ArbFiniteDuration.*
import explore.model.arb.ArbFiniteDuration.given
import explore.model.formats.*
import lucuma.core.model.NonNegDuration
import lucuma.core.model.arb.ArbNonNegDuration.*
import lucuma.core.optics.laws.discipline.ValidSplitEpiTests
import lucuma.core.optics.laws.discipline.ValidWedgeTests
import org.scalacheck.Prop.*
import org.typelevel.cats.time.arb.TimeArbitraries.*
import org.typelevel.cats.time.instances.all.*

import java.time.Duration

class FormatsSuite extends munit.DisciplineSuite {
  assertEquals(parsers.durationHM.parseAll("0").toOption,
               NonNegDuration.unsafeFrom(Duration.ofMinutes(0)).some
  )
  assertEquals(parsers.durationHM.parseAll("0:0").toOption,
               NonNegDuration.unsafeFrom(Duration.ofMinutes(0)).some
  )
  assertEquals(parsers.durationHM.parseAll("0:00").toOption,
               NonNegDuration.unsafeFrom(Duration.ofMinutes(0)).some
  )
  assertEquals(parsers.durationHM.parseAll("0:2").toOption,
               NonNegDuration.unsafeFrom(Duration.ofMinutes(2)).some
  )
  assertEquals(parsers.durationHM.parseAll("0:48").toOption,
               NonNegDuration.unsafeFrom(Duration.ofMinutes(48)).some
  )
  checkAll(
    "durationHMFormat",
    ValidWedgeTests(durationHM).validWedgeLawsWith(finiteDurationsHM)
  )
}
