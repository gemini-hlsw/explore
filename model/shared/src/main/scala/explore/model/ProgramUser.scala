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
// An empty OrcidProfile implies a guest - which is possible for PIs, but
// not other program roles.
case class ProgramUser(
  id:      User.Id,
  profile: Option[OrcidProfile]
) derives Eq

object ProgramUser:
  private def toOrcidId(s: String): Decoder.Result[OrcidId] =
    OrcidId.fromValue(s).fold(s => DecodingFailure(s, Nil).asLeft, _.asRight)

  given Decoder[ProgramUser] = c =>
    c.downField("type").as[String].flatMap {
      case "STANDARD" =>
        for {
          id      <- c.downField("id").as[User.Id]
          orcidId <- c.downField("orcidId").as[String].flatMap(toOrcidId)
          gname   <- c.downField("orcidGivenName").as[Option[String]]
          fname   <- c.downField("orcidFamilyName").as[Option[String]]
          cname   <- c.downField("orcidCreditName").as[Option[String]]
          email   <- c.downField("orcidEmail").as[Option[String]]
        } yield ProgramUser(id, OrcidProfile(orcidId, gname, fname, cname, email).some)

      case "GUEST" =>
        for {
          id <- c.downField("id").as[User.Id]
        } yield ProgramUser(id, none)

      case tag =>
        DecodingFailure(s"Invalid program user type: $tag", Nil).asLeft
    }

  // The API should prevent non-std users from creating proposals, then we
  // can use the code below and make the profile non-optional.

  // private def validateUserType(s: String): Decoder.Result[Unit] =
  //   if (s === "STANDARD") ().asRight
  //   else DecodingFailure(s"Invalid program user type `$s`", Nil).asLeft

  // given Decoder[ProgramUser] = Decoder.instance(c =>
  //   for {
  //     _       <- c.downField("type").as[String].flatMap(validateUserType)
  //     id      <- c.downField("id").as[User.Id]
  //     orcidId <- c.downField("orcidId").as[String].flatMap(toOrcidId)
  //     gname   <- c.downField("orcidGivenName").as[Option[String]]
  //     cname   <- c.downField("orcidCreditName").as[Option[String]]
  //     fname   <- c.downField("orcidFamilyName").as[Option[String]]
  //     email   <- c.downField("orcidEmail").as[Option[String]]
  //   } yield ProgramUser(id, OrcidProfile(orchidId, gname, cname, fname, email))
  // )
