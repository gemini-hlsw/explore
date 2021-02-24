// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import explore.model.enum._
import monocle.macros.Lenses

import java.util.UUID

@Lenses
final case class Constraints(
  id:   Constraints.Id,
  name: String,
  cc:   CloudCover,
  iq:   ImageQuality,
  sb:   SkyBackground,
  wv:   WaterVapor
) {
  override def toString: String =
    s"""$productPrefix(UUID.fromString("$id")
       |"$name"
       |CloudCover.$cc
       |ImageQuality.$iq
       |SkyBackground.$sb
       |WaterVapor.$wv)""".stripMargin.replaceAll("\n", ", ")
}

object Constraints {
  type Id = UUID

  implicit val equalConstraints: Eq[Constraints] =
    Eq.by(x => (x.id, x.name, x.cc, x.iq, x.sb, x.wv))

  implicit val showConstraints: Show[Constraints] = Show.fromToString

}
