// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import explore.model.enums.ProgramUserRole
import io.circe.Decoder
import lucuma.odb.data.PartnerLink
import lucuma.odb.json.partnerlink.given

// an empty role implies PI
case class ProgramUserWithRole(
  user:        ProgramUser,
  partnerLink: Option[PartnerLink],
  role:        Option[ProgramUserRole]
) derives Decoder,
      Eq {
  val roleName: String = role match {
    case None       => "Pi"
    case Some(role) => role.tag
  }

  val name: String = user.profile.fold("Guest User")(p => p.displayName)
}
