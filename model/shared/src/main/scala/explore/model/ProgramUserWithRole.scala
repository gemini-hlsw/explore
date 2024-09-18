// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.Gender
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.PartnerLink
import lucuma.odb.json.partnerlink.given
import monocle.Focus
import monocle.Lens

case class ProgramUserWithRole(
  user:              ProgramUser,
  partnerLink:       Option[PartnerLink],
  role:              ProgramUserRole,
  educationalStatus: Option[EducationalStatus],
  thesis:            Option[Boolean],
  gender:            Option[Gender]
) derives Eq:
  export user.{name, nameWithEmail}

object ProgramUserWithRole:
  val user: Lens[ProgramUserWithRole, ProgramUser] = Focus[ProgramUserWithRole](_.user)

  val partnerLink: Lens[ProgramUserWithRole, Option[PartnerLink]] =
    Focus[ProgramUserWithRole](_.partnerLink)

  val educationalStatus: Lens[ProgramUserWithRole, Option[EducationalStatus]] =
    Focus[ProgramUserWithRole](_.educationalStatus)

  val thesis: Lens[ProgramUserWithRole, Option[Boolean]] =
    Focus[ProgramUserWithRole](_.thesis)

  val gender: Lens[ProgramUserWithRole, Option[Gender]] =
    Focus[ProgramUserWithRole](_.gender)

  given Decoder[ProgramUserWithRole] = c =>
    for {
      u    <- c.downField("user").as[ProgramUser]
      pl   <- c.downField("partnerLink").as[Option[PartnerLink]]
      role <- c.downField("role").as[ProgramUserRole]
      es   <- c.downField("educationalStatus").as[Option[EducationalStatus]]
      th   <- c.downField("thesis").as[Option[Boolean]]
      g    <- c.downField("gender").as[Option[Gender]]
    } yield ProgramUserWithRole(u, pl, role, es, th, g)
