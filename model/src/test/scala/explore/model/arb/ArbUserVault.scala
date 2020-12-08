// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.UserVault
import lucuma.core.model.arb._
import lucuma.core.arb._
import eu.timepit.refined.scalacheck.string._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import eu.timepit.refined.types.string.NonEmptyString
import java.time.Instant
import lucuma.core.model.User
import sttp.model._
import sttp.client3._

trait ArbUserVault {
  import ArbUser._
  import ArbTime._
  // let's use a fixed value it doesn't matter for the test and there are no arbitrary instances for Uri
  val host: Uri = uri"https://localhost"

  implicit val userVaultArb = Arbitrary[UserVault] {
    for {
      user  <- arbitrary[User]
      exp   <- arbitrary[Instant]
      token <- arbitrary[NonEmptyString]
    } yield UserVault(user, host, exp, token)
  }

  implicit def userVaultCogen: Cogen[UserVault] =
    Cogen[(User, Instant, String)].contramap(m => (m.user, m.expiration, m.token.value))
}

object ArbUserVault extends ArbUserVault
