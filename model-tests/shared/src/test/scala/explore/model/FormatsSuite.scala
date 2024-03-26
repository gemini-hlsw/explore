// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import eu.timepit.refined.cats.given
import explore.model.formats.*
import lucuma.core.arb.*
import lucuma.core.optics.laws.discipline.ValidWedgeTests
import lucuma.core.optics.laws.discipline.FormatTests
import explore.model.formats.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan.given
import lucuma.core.math.Angle
import lucuma.core.math.arb.ArbAngle.given
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import java.time.Duration

class FormatsSuite extends munit.DisciplineSuite {
  private val perturbations: List[String => Gen[String]] =
    List(_ => arbitrary[String]) // swap for a random string

  val finiteDurationsHM: Gen[String] =
    arbitrary[TimeSpan]
      .map { ts =>
        s"${ts.toHoursPart}:${ts.toMinutesPart}"
      }
      .flatMapOneOf(Gen.const, perturbations*)

  val finiteDurationsHMS: Gen[String] =
    arbitrary[TimeSpan]
      .map { ts =>
        val secs =
          if (ts.toMillisPart > 0)
            f"${ts.toSecondsPart}%02d.${ts.toMillisPart}%03d"
          else
            f"${ts.toSecondsPart}%02d"
        s"${ts.toHoursPart}:${ts.toMinutes}:$secs"
      }
      .flatMapOneOf(Gen.const, (((_: String) => finiteDurationsHM) :: perturbations)*)

  assertEquals(parsers.durationHM.parseAll("0").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMinutes(0)).some
  )
  assertEquals(parsers.durationHM.parseAll("0:0").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMinutes(0)).some
  )
  assertEquals(parsers.durationHM.parseAll("0:00").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMinutes(0)).some
  )
  assertEquals(parsers.durationHM.parseAll("0:2").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMinutes(2)).some
  )
  assertEquals(parsers.durationHM.parseAll("0:48").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMinutes(48)).some
  )
  assertEquals(
    parsers.durationHMS.parseAll("18179:02:26.000").toOption,
    TimeSpan.unsafeFromDuration(Duration.ofHours(18179).plusMinutes(2).plusSeconds(26)).some
  )
  assertEquals(
    parsers.durationHMS.parseAll("18179:02:09.033").toOption,
    TimeSpan
      .unsafeFromDuration(Duration.ofHours(18179).plusMinutes(2).plusSeconds(9).plusMillis(33))
      .some
  )
  assertEquals(
    parsers.durationHMS.parseAll("18179:02:09.0319029201091").toOption,
    TimeSpan
      .unsafeFromDuration(Duration.ofHours(18179).plusMinutes(2).plusSeconds(9).plusMillis(31))
      .some
  )
  checkAll(
    "durationHMValidWedge",
    ValidWedgeTests(durationHM).validWedgeLawsWith(finiteDurationsHM)
  )
  checkAll(
    "durationHMSValidWedge",
    ValidWedgeTests(durationHMS).validWedgeLawsWith(finiteDurationsHMS)
  )
}
