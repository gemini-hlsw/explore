// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.User
import lucuma.core.model.OrcidId
import lucuma.core.model.UserProfile
import lucuma.core.model.arb.ArbOrcidId.given
import lucuma.core.model.arb.ArbUserProfile.given
import lucuma.core.util.arb.ArbGid.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbUser:
  given Arbitrary[User] =
    Arbitrary {
      for {
        id      <- arbitrary[User.Id]
        orcidId <- arbitrary[Option[OrcidId]]
        profile <- arbitrary[Option[UserProfile]]
      } yield User(id, orcidId, profile)
    }

  given Cogen[User] =
    Cogen[
      (
        User.Id,
        Option[OrcidId],
        Option[UserProfile]
      )
    ].contramap(u => (u.id, u.orcidId, u.profile))

object ArbUser extends ArbUser
