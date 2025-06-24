// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.ProgramUser
import explore.model.User
import explore.model.UserInvitation
import explore.model.arb.ArbUser.given
import explore.model.arb.ArbUserInvitation.given
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.Gender
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.PartnerLink
import lucuma.core.model.UserProfile
import lucuma.core.model.arb.ArbPartnerLink.given
import lucuma.core.model.arb.ArbUserProfile.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbProgramUser:
  given Arbitrary[ProgramUser] =
    Arbitrary {
      for {
        id                <- arbitrary[ProgramUser.Id]
        user              <- arbitrary[Option[User]]
        partnerLink       <- arbitrary[Option[PartnerLink]]
        role              <- arbitrary[ProgramUserRole]
        educationalStatus <- arbitrary[Option[EducationalStatus]]
        thesis            <- arbitrary[Option[Boolean]]
        gender            <- arbitrary[Option[Gender]]
        fallbackProfile   <- arbitrary[UserProfile]
        invitations       <- arbitrary[List[UserInvitation]]
        hasDataAccess     <- arbitrary[Boolean]
      } yield ProgramUser(id,
                          user,
                          partnerLink,
                          role,
                          educationalStatus,
                          thesis,
                          gender,
                          fallbackProfile,
                          invitations,
                          hasDataAccess
      )
    }

  given Cogen[ProgramUser] =
    Cogen[
      (
        ProgramUser.Id,
        Option[User],
        Option[PartnerLink],
        ProgramUserRole,
        Option[EducationalStatus],
        Option[Boolean],
        Option[Gender],
        UserProfile,
        List[UserInvitation],
        Boolean
      )
    ].contramap(u =>
      (u.id,
       u.user,
       u.partnerLink,
       u.role,
       u.educationalStatus,
       u.thesis,
       u.gender,
       u.fallbackProfile,
       u.invitations,
       u.hasDataAccess
      )
    )

object ArbProgramUser extends ArbProgramUser
