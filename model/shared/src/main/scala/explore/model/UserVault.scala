// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.User
import monocle.Focus
import org.typelevel.cats.time.instances.instant._

import java.time.Instant

final case class UserVault(user: User, expiration: Instant, token: NonEmptyString)

object UserVault {
  val user = Focus[UserVault](_.user)

  implicit val eqUserVault: Eq[UserVault] =
    Eq.by(x => (x.user, x.expiration, x.token))

}
