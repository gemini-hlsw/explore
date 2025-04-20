// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import java.util.UUID

import org.scalacheck.Cogen

trait CogenUUID {
  implicit val cogenUUID: Cogen[UUID] =
    Cogen[(Long, Long)].contramap(u => (u.getMostSignificantBits, u.getLeastSignificantBits))
}

object CogenUUID extends CogenUUID
