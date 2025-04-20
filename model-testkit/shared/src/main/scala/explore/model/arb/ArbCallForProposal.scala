// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.arb.ArbSemester.given
import lucuma.core.model.arb.ArbCallCoordinatesLimits.given
import lucuma.core.util.arb.ArbDateInterval.given
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbGid.given
import lucuma.core.util.arb.ArbTimestamp.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

import explore.model.CallForProposal
import explore.model.CallPartner
import lucuma.core.enums.CallForProposalsType
import lucuma.core.enums.Instrument
import lucuma.core.enums.Partner
import lucuma.core.model.CallForProposals
import lucuma.core.model.Semester
import lucuma.core.util.Timestamp
import lucuma.core.model.CallCoordinatesLimits
import lucuma.core.util.DateInterval

trait ArbCallForProposal {
  import ArbEnumerated.given

  given Arbitrary[CallPartner] =
    Arbitrary {
      for {
        partner  <- arbitrary[Partner]
        deadline <- arbitrary[Option[Timestamp]]
      } yield CallPartner(partner, deadline)
    }

  given Cogen[CallPartner] =
    Cogen[(Partner, Option[Timestamp])].contramap(p => (p.partner, p.submissionDeadline))

  given Arbitrary[CallForProposal] =
    Arbitrary {
      for {
        id       <- arbitrary[CallForProposals.Id]
        semester <- arbitrary[Semester]
        title    <- arbitrary[NonEmptyString]
        cfpType  <- arbitrary[CallForProposalsType]
        partners <- arbitrary[List[CallPartner]]
        deadline <- arbitrary[Option[Timestamp]]
        instr    <- arbitrary[List[Instrument]]
        limits   <- arbitrary[CallCoordinatesLimits]
        active   <- arbitrary[DateInterval]
      } yield CallForProposal(id,
                              semester,
                              title,
                              cfpType,
                              partners,
                              deadline,
                              instr,
                              limits,
                              active
      )
    }

  given Cogen[CallForProposal] =
    Cogen[
      (CallForProposals.Id,
       Semester,
       NonEmptyString,
       CallForProposalsType,
       List[CallPartner],
       Option[Timestamp],
       List[Instrument],
       CallCoordinatesLimits,
       DateInterval
      )
    ].contramap: p =>
      (p.id,
       p.semester,
       p.title,
       p.cfpType,
       p.partners,
       p.nonPartnerDeadline,
       p.instruments,
       p.coordinateLimits,
       p.active
      )
}

object ArbCallForProposal extends ArbCallForProposal
