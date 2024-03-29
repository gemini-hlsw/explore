// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats.given
import eu.timepit.refined.string.MatchesRegex
import io.circe.Decoder

// Matchtes the odb standard for email validation
type EmailPred    = MatchesRegex[
  """(?:[a-z0-9!#$%&'*+x\/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+\/=?^_`{|}~-]+)*|"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])"""
]
type RefinedEmail = String Refined EmailPred

case class UserInvitation(id: String, email: RefinedEmail) derives Eq

object UserInvitation:
  given Decoder[UserInvitation] = c =>
    for {
      id <- c.get[String]("id")
      em <- c.get[String]("recipientEmail").map(Refined.unsafeApply[String, EmailPred])
    } yield UserInvitation(id, em)
