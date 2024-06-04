// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import explore.model.ProposalType
import explore.model.arb.ArbPartnerSplit.given
import explore.model.arb.ArbProposalType.given
import explore.optics.all.*
import lucuma.core.math.arb.ArbRadialVelocity
import lucuma.core.model.arb.ArbTarget
import lucuma.core.util.arb.ArbEnumerated.given
import monocle.law.discipline.OptionalTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary.*

class ModelOpticsSuite extends DisciplineSuite:
  import ArbRadialVelocity.given
  import ArbTarget.given

  checkAll("targetRV", OptionalTests(TargetRV))
  checkAll("toOActivation", OptionalTests(ProposalType.toOActivation))
  checkAll("partnerSplits", OptionalTests(ProposalType.partnerSplits))
