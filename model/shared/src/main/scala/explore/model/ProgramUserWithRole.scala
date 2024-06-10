// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import explore.model.enums.ProgramUserRole
import io.circe.Decoder
import lucuma.core.enums.Partner

// an empty role implies PI
case class ProgramUserWithRole(
  user:    ProgramUser,
  partner: Option[Partner],
  role:    Option[ProgramUserRole]
) derives Decoder,
      Eq {
  val roleName: String = role match {
    case None       => "Pi"
    case Some(role) => role.tag
  }

  val name: String = user.profile.fold("Guest User")(p => p.displayName)
}
