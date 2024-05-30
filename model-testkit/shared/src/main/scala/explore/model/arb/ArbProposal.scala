// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.TacCategory
import explore.model.Proposal
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

import explore.model.arb.ArbProposalType.given
import explore.model.ProposalType
import lucuma.core.model.CallForProposals

trait ArbProposal:

  given Arbitrary[Proposal] =
    Arbitrary {
      for {
        callId       <- arbitrary[Option[CallForProposals.Id]]
        title        <- arbitrary[Option[NonEmptyString]]
        category     <- arbitrary[Option[TacCategory]]
        abstrakt     <- arbitrary[Option[NonEmptyString]]
        proposalType <- arbitrary[Option[ProposalType]]
      } yield Proposal(callId, title, category, abstrakt, proposalType)
    }

  given Cogen[Proposal] =
    Cogen[
      (
        Option[CallForProposals.Id],
        Option[NonEmptyString],
        Option[TacCategory],
        Option[NonEmptyString],
        Option[ProposalType]
      )
    ].contramap(p => (p.cfpId, p.title, p.category, p.abstrakt, p.proposalType))

object ArbProposal extends ArbProposal
