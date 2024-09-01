// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import explore.model.enums.ProgramUserRole
import io.circe.Decoder
import lucuma.core.enums.EducationalStatus
import lucuma.core.model.PartnerLink
import lucuma.odb.json.partnerlink.given
import monocle.Focus
import monocle.Lens

// an empty role implies PI
case class ProgramUserWithRole(
  user:              ProgramUser,
  partnerLink:       Option[PartnerLink],
  role:              Option[ProgramUserRole],
  educationalStatus: Option[EducationalStatus],
  thesis:            Option[Boolean]
) derives Eq:
  val roleName: String = role match {
    case None       => "Pi"
    case Some(role) => role.tag
  }

  val name: String = user.profile.fold("Guest User")(p => p.displayName)

object ProgramUserWithRole:
  val user: Lens[ProgramUserWithRole, ProgramUser] = Focus[ProgramUserWithRole](_.user)

  val partnerLink: Lens[ProgramUserWithRole, Option[PartnerLink]] =
    Focus[ProgramUserWithRole](_.partnerLink)

  val educationalStatus: Lens[ProgramUserWithRole, Option[EducationalStatus]] =
    Focus[ProgramUserWithRole](_.educationalStatus)

  val thesis: Lens[ProgramUserWithRole, Option[Boolean]] =
    Focus[ProgramUserWithRole](_.thesis)

  given Decoder[ProgramUserWithRole] = c =>
    for {
      u    <- c.downField("user").as[ProgramUser]
      pl   <- c.downField("partnerLink").as[Option[PartnerLink]]
      role <- c.downField("role").as[Option[ProgramUserRole]]
      es   <- c.downField("educationalStatus").as[Option[EducationalStatus]]
      th   <- c.downField("thesis").as[Option[Boolean]]
    } yield ProgramUserWithRole(u, pl, role, es, th)
