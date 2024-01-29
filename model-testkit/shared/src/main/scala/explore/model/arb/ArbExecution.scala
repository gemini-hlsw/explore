// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.Execution
import explore.model.TimeCharge
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.arb.ArbExecutionDigest.given
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

trait ArbExecution:

  given Arbitrary[TimeCharge] = Arbitrary(
    for {
      program <- arbitrary[TimeSpan]
    } yield TimeCharge(program)
  )

  given Arbitrary[Execution] = Arbitrary(
    for {
      digest     <- arbitrary[Option[ExecutionDigest]]
      timeCharge <- arbitrary[TimeCharge]
    } yield Execution(digest, timeCharge)
  )

object ArbExecution extends ArbExecution
