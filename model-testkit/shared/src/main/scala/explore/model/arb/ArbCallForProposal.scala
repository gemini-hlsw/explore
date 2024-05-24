// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.arb.ArbSemester.given
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

import explore.model.CallForProposal
import explore.model.CallForProposalType
import lucuma.core.model.Semester

trait ArbCallForProposal {
  import ArbEnumerated.given

  given Arbitrary[CallForProposal] =
    Arbitrary {
      for {
        semester <- arbitrary[Semester]
        title    <- arbitrary[NonEmptyString]
        cfpType  <- arbitrary[CallForProposalType]
      } yield CallForProposal(semester, title, cfpType)
    }

  given Cogen[CallForProposal] =
    Cogen[
      (Semester, NonEmptyString, CallForProposalType)
    ].contramap(p => (p.semester, p.title, p.cfpType))
}

object ArbCallForProposal extends ArbCallForProposal
