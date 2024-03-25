// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder

case class UserInvitation(id: String, email: String) derives Eq

object UserInvitation:
  given Decoder[UserInvitation] = c =>
    for {
      id <- c.get[String]("id")
      em <- c.get[String]("email")
    } yield UserInvitation(id, em)
