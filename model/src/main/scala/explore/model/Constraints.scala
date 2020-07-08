// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import java.util.UUID

import cats._
import cats.implicits._
import explore.model.enum._
import monocle.macros.Lenses

@Lenses
final case class Constraints(
  id:   Constraints.Id,
  name: String,
  cc:   CloudCover,
  iq:   ImageQuality,
  sb:   SkyBackground,
  wv:   WaterVapor
)

object Constraints {
  type Id = UUID

  implicit val equalConstraints: Eq[Constraints] =
    Eq.by(x => (x.id, x.name, x.cc, x.iq, x.sb, x.wv))

  implicit val showConstraints: Show[Constraints] =
    Show.show(x => s"$name (${List(x.cc, x.iq, x.sb, x.wv).mkString(", ")}")

}
