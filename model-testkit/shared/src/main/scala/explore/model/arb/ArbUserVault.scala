// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.UserVault
import lucuma.core.model.arb.*
import lucuma.core.arb.*
import eu.timepit.refined.scalacheck.string.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import eu.timepit.refined.types.string.NonEmptyString
import java.time.Instant
import lucuma.core.model.User

trait ArbUserVault {
  import ArbUser.*
  import ArbTime.*

  implicit val userVaultArb: Arbitrary[UserVault] = Arbitrary[UserVault] {
    for {
      user  <- arbitrary[User]
      exp   <- arbitrary[Instant]
      token <- arbitrary[NonEmptyString]
    } yield UserVault(user, exp, token)
  }

  implicit def userVaultCogen: Cogen[UserVault] =
    Cogen[(User, Instant, String)].contramap(m => (m.user, m.expiration, m.token.value))
}

object ArbUserVault extends ArbUserVault
