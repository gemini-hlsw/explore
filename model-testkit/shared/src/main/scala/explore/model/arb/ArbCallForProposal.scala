// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.arb.ArbSemester.given
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbGid.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

import explore.model.CallForProposal
import explore.model.CallPartner
import lucuma.core.enums.CallForProposalsType
import lucuma.core.enums.Partner
import lucuma.core.model.CallForProposals
import lucuma.core.model.Semester

trait ArbCallForProposal {
  import ArbEnumerated.given

  given Arbitrary[CallForProposal] =
    Arbitrary {
      for {
        id       <- arbitrary[CallForProposals.Id]
        semester <- arbitrary[Semester]
        title    <- arbitrary[NonEmptyString]
        cfpType  <- arbitrary[CallForProposalsType]
        partners <- arbitrary[List[Partner]]
      } yield CallForProposal(id, semester, title, cfpType, partners.map(CallPartner(_)))
    }

  given Cogen[CallForProposal] =
    Cogen[
      (CallForProposals.Id, Semester, NonEmptyString, CallForProposalsType, List[Partner])
    ].contramap(p => (p.id, p.semester, p.title, p.cfpType, p.partners.map(_.partner)))
}

object ArbCallForProposal extends ArbCallForProposal
