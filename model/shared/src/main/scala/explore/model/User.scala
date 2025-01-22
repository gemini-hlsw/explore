// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.model.OrcidId
import lucuma.core.model.UserProfile
import lucuma.sso.client.codec.userProfile.given

// Case class for the 'pi' and `users` in the Program. These must be
// actual users, not service users. But, can be guests.
case class User(
  id:      User.Id,
  orcidId: Option[OrcidId],
  profile: Option[UserProfile]
) derives Eq:
  lazy val name: String = profile.fold("Guest User")(_.displayName.orEmpty)

  lazy val lastName: String = profile.flatMap(p => p.familyName.orElse(p.displayName)).orEmpty

  lazy val nameWithEmail: String =
    name + profile.flatMap(_.email).foldMap(email => s" ($email)")

object User:
  type Id = lucuma.core.model.User.Id
  val Id = lucuma.core.model.User.Id

  given Decoder[User] = c =>
    for {
      id      <- c.downField("id").as[User.Id]
      orcidId <- c.downField("orcidId").as[Option[OrcidId]]
      profile <- c.downField("profile").as[Option[UserProfile]]
    } yield User(id, orcidId, profile)
