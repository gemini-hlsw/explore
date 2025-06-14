// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.Execution
import explore.model.ProgramTime
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.arb.ArbExecutionDigest.given
import lucuma.core.util.CalculatedValue
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbCalculatedValue.given
import lucuma.core.util.arb.ArbTimeSpan.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbExecution:

  given Arbitrary[Execution] = Arbitrary(
    for {
      digest     <- arbitrary[CalculatedValue[Option[ExecutionDigest]]]
      timeCharge <- arbitrary[TimeSpan]
    } yield Execution(digest, ProgramTime(timeCharge))
  )

  given Cogen[Execution] = Cogen[
    (CalculatedValue[Option[ExecutionDigest]], TimeSpan)
  ].contramap(e => (e.digest, e.programTimeCharge.value))

object ArbExecution extends ArbExecution
