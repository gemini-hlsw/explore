// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.*
import lucuma.core.model.Program

case class ProgramInvitation(givenName: String, familyName: String, pid: Program.Id) derives Eq

object ProgramInvitation:
  given Decoder[ProgramInvitation] = c =>
    for {
      gn <- c.downField("issuer").get[String]("orcidGivenName")
      fn <- c.downField("issuer").get[String]("orcidFamilyName")
      pi <- c.downField("program").get[Program.Id]("id")
    } yield ProgramInvitation(gn, fn, pi)
