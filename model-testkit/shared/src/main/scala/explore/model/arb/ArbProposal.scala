// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.CallForProposal
import explore.model.Proposal
import explore.model.ProposalType
import explore.model.arb.ArbCallForProposal.given
import explore.model.arb.ArbProposalType.given
import lucuma.core.enums.TacCategory
import lucuma.core.model.ProposalReference
import lucuma.core.model.arb.ArbProposalReference.given
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbProposal:

  given Arbitrary[Proposal] =
    Arbitrary {
      for {
        call         <- arbitrary[Option[CallForProposal]]
        category     <- arbitrary[Option[TacCategory]]
        proposalType <- arbitrary[Option[ProposalType]]
        reference    <- arbitrary[Option[ProposalReference]]
      } yield Proposal(call, category, proposalType, reference)
    }

  given Cogen[Proposal] =
    Cogen[
      (
        Option[CallForProposal],
        Option[TacCategory],
        Option[ProposalType],
        Option[ProposalReference]
      )
    ].contramap(p => (p.call, p.category, p.proposalType, p.reference))

object ArbProposal extends ArbProposal
