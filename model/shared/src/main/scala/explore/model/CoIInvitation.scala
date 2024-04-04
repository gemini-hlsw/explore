// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.string.MatchesRegex
import io.circe.*
import io.circe.refined.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.InvitationStatus
import lucuma.core.util.Enumerated

case class CoIInvitation(id: String, email: EmailAddress, status: InvitationStatus) derives Eq

object CoIInvitation:
  given Decoder[CoIInvitation] = c =>
    for {
      id <- c.get[String]("id")
      em <- c.get[EmailAddress]("recipientEmail")
      s  <- c.get[InvitationStatus]("status")
    } yield CoIInvitation(id, em, s)
