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

trait ArbFiniteDuration {

  // given Arbitrary[FiniteDuration] = {
  //   import TimeUnit.*
  //
  //   val genTU = Gen.oneOf(NANOSECONDS, MICROSECONDS, MILLISECONDS, SECONDS, MINUTES, HOURS)
  //
  //   Arbitrary {
  //     genTU.flatMap(u => Gen.choose[Long](0L, 60L).map(FiniteDuration(_, u)))
  //   }
  // }
  //
  // given Cogen[FiniteDuration] =
  //   Cogen[Long].contramap(_.toNanos)
  //
  // // private[this] val blundedFiniteDurationHM: Gen[FiniteDuration] =
  //   for {
  //     s  <- Gen.choose(0, 60)
  //     bd <- arbitrary[Int].map(BigDecimal(_))
  //   } yield BigDecimal(bd.underlying.movePointLeft(s))
  private val perturbations: List[String => Gen[String]] =
    List(_ => arbitrary[String] // swap for a random string
    // s => Gen.const(s.replace(":", " ")) // replace colons with spaces (ok)
    )

  val finiteDurationsHM: Gen[String] =
    arbitrary[NonNegDuration]
      .map { d =>
        // println(s"${d.toMinutes / 60}:${d.toMinutes % 60}")
        s"${d.value.toHours / 60}:${d.value.toMinutes % 60}"
      }
      .flatMapOneOf(Gen.const, perturbations: _*)
//     Gen.oneOf(intBoundedBigDecimals, arbitrary[BigDecimal])
}

object ArbFiniteDuration extends ArbFiniteDuration
