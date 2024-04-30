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
import lucuma.core.util.Enumerated

case class CoIInvitation(
  id:          String,
  email:       EmailAddress,
  status:      InvitationStatus,
  emailStatus: Option[EmailStatus]
) derives Eq

object CoIInvitation:
  given Decoder[CoIInvitation] = c =>
    for {
      id <- c.get[String]("id")
      em <- c.get[EmailAddress]("recipientEmail")
      s  <- c.get[InvitationStatus]("status")
      es <- c.downField("email").downField("status").success.traverse(_.as[EmailStatus])
    } yield CoIInvitation(id, em, s, es)
