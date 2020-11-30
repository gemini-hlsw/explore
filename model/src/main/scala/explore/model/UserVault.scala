// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import java.time.Instant

import cats.Eq
import cats.implicits._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.chrisdavenport.cats.time.instances.instant._
import lucuma.core.model.User
import monocle.macros.Lenses
import sttp.model.Uri

@Lenses
final case class UserVault(user: User, ssoURI: Uri, expiration: Instant, token: NonEmptyString)

object UserVault {
  implicit val eqUserVault: Eq[UserVault] =
    Eq.by(x => (x.user, x.ssoURI.toString, x.expiration, x.token))

}
