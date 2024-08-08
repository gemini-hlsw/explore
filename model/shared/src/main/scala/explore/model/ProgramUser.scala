// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.DecodingFailure
import lucuma.core.model.OrcidId
import lucuma.core.model.OrcidProfile
import lucuma.core.model.User

// Case class for the 'pi' and `users` in the Program. These must be
// actual users, not service users.
case class ProgramUser(
  id:      User.Id,
  profile: Option[OrcidProfile]
) derives Eq
// lazy val name: String = profile.fold("Guest User")(p => p.displayName)

// lazy val nameWithEmail: String =
//   name + profile.flatMap(_.primaryEmail).foldMap(email => s" ($email)")

object ProgramUser:
  private def toOrcidId(s: String): Decoder.Result[OrcidId] =
    OrcidId.fromValue(s).fold(s => DecodingFailure(s, Nil).asLeft, _.asRight)

  given Decoder[ProgramUser] = c =>
    for {
      id      <- c.downField("id").as[User.Id]
      orcidId <- c.downField("orcidId").as[Option[String]].map(_.traverse(toOrcidId))
      gname   <- c.downField("orcidGivenName").as[Option[String]]
      fname   <- c.downField("orcidFamilyName").as[Option[String]]
      cname   <- c.downField("orcidCreditName").as[Option[String]]
      email   <- c.downField("orcidEmail").as[Option[String]]
    } yield ProgramUser(id,
                        orcidId.toOption.flatten.map(OrcidProfile(_, gname, fname, cname, email))
    )
