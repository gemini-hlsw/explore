// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import lucuma.core.arb.*
import java.time.Duration
import org.typelevel.cats.time.arb.TimeArbitraries.*
import lucuma.core.model.NonNegDuration
import lucuma.core.model.arb.ArbNonNegDuration.*
import java.time.temporal.TemporalUnit

trait ArbFiniteDuration {

  private val perturbations: List[String => Gen[String]] =
    List(_ => arbitrary[String]) // swap for a random string

  val finiteDurationsHM: Gen[String] =
    arbitrary[NonNegDuration]
      .map { d =>
        s"${d.value.toHoursPart}:${d.value.toMinutesPart}"
      }
      .flatMapOneOf(Gen.const, perturbations: _*)

  val finiteDurationsHMS: Gen[String] =
    arbitrary[NonNegDuration]
      .map { d =>
        val secs =
          if (d.value.toMillisPart() > 0)
            f"${d.value.toSecondsPart()}%02d.${d.value.toMillisPart() % 1000}%03d"
          else
            f"${d.value.toSecondsPart()}%02d"
        s"${d.value.toHoursPart}:${d.value.toMinutesPart}:$secs"
      }
      .flatMapOneOf(Gen.const, (((_: String) => finiteDurationsHM) :: perturbations): _*)
}

object ArbFiniteDuration extends ArbFiniteDuration
