// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.UserInvitation
import lucuma.core.data.EmailAddress
import lucuma.core.data.arb.ArbEmailAddress.given
import lucuma.core.enums.EmailStatus
import lucuma.core.enums.InvitationStatus
import lucuma.core.util.arb.ArbEnumerated.given;
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbUserInvitation {
  given Arbitrary[UserInvitation] =
    Arbitrary {
      for {
        id <- arbitrary[String]
        em <- arbitrary[EmailAddress]
        s  <- arbitrary[InvitationStatus]
        es <- arbitrary[Option[EmailStatus]]
      } yield UserInvitation(id, em, s, es)
    }

  given Cogen[UserInvitation] =
    Cogen[
      (
        String,
        EmailAddress,
        InvitationStatus,
        Option[EmailStatus]
      )
    ].contramap(u => (u.id, u.email, u.status, u.emailStatus))
}

object ArbUserInvitation extends ArbUserInvitation
