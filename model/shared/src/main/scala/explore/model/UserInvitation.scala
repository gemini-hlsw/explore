// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats.given
import eu.timepit.refined.string.MatchesRegex
import io.circe.Decoder
import lucuma.core.util.Enumerated

// Matchtes the odb standard for email validation
type EmailPred =
  MatchesRegex["""^[a-zA-Z0-9_+&-]+(?:.[a-zA-Z0-9_+&-]+)*@(?:[a-zA-Z0-9-]+.)+[a-zA-Z]{2,7}$"""]

type RefinedEmail = String Refined EmailPred

// Move to lucuma-core
enum InvitationStatus(val tag: String) derives Enumerated:
  case Pending  extends InvitationStatus("pending")
  case Redeemed extends InvitationStatus("redeemed")
  case Declined extends InvitationStatus("declined")
  case Revoked  extends InvitationStatus("revoked")

// TODO: move to lucuma-core
case class UserInvitation(id: String, email: RefinedEmail, status: InvitationStatus) derives Eq

object UserInvitation:
  given Decoder[UserInvitation] = c =>
    for {
      id <- c.get[String]("id")
      em <- c.get[String]("recipientEmail").map(Refined.unsafeApply[String, EmailPred])
      s  <- c.get[InvitationStatus]("status")
    } yield UserInvitation(id, em, s)
