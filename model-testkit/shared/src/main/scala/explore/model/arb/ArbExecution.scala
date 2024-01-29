// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.Execution
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.arb.ArbCategorizedTime.given
import lucuma.core.model.sequence.arb.ArbExecutionDigest.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

trait ArbExecution:

  given Arbitrary[Execution] = Arbitrary(
    for {
      digest     <- arbitrary[Option[ExecutionDigest]]
      timeCharge <- arbitrary[CategorizedTime]
    } yield Execution(digest, timeCharge)
  )

object ArbExecution extends ArbExecution
