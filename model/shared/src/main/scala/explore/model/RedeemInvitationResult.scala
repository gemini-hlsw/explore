// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.*
import lucuma.core.model.Program

case class RedeemInvitationResult(givenName: String, familyName: String, pid: Program.Id) derives Eq

object RedeemInvitationResult:
  given Decoder[RedeemInvitationResult] = c =>
    for {
      gn <- c.downField("issuer").downField("profile").get[String]("givenName")
      fn <- c.downField("issuer").downField("profile").get[String]("familyName")
      pi <- c.downField("programUser").downField("program").get[Program.Id]("id")
    } yield RedeemInvitationResult(gn, fn, pi)
