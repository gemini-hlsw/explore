// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.string.MatchesRegex
import io.circe.*
import io.circe.refined.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.EmailStatus
import lucuma.core.enums.InvitationStatus
import lucuma.core.enums.ProgramUserRole
import lucuma.core.util.Enumerated

case class UserInvitation(
  id:          String,
  email:       EmailAddress,
  role:        ProgramUserRole,
  status:      InvitationStatus,
  emailStatus: Option[EmailStatus]
) derives Eq

object UserInvitation:
  given Decoder[UserInvitation] = c =>
    for {
      id <- c.get[String]("id")
      em <- c.get[EmailAddress]("recipientEmail")
      r  <- c.get[ProgramUserRole]("role")
      s  <- c.get[InvitationStatus]("status")
      es <- c.downField("email").downField("status").success.traverse(_.as[EmailStatus])
    } yield UserInvitation(id, em, r, s, es)
