// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.implicits._
import explore.model.enum._
import monocle.macros.Lenses

@Lenses
final case class Constraints(
  cc: CloudCover,
  iq: ImageQuality,
  sb: SkyBackground,
  wv: WaterVapor
)

object Constraints {

  val Worst: Constraints =
    Constraints(
      CloudCover.Any,
      ImageQuality.Any,
      SkyBackground.Any,
      WaterVapor.Any
    )

  val Nominal: Constraints =
    Constraints(
      CloudCover.Percent50,
      ImageQuality.Percent70,
      SkyBackground.Percent50,
      WaterVapor.Any
    )

  val Best: Constraints =
    Constraints(
      // In the ODB model it's 20% but that value it's marked as obsolete
      // so I took the non-obsolete lowest value.
      CloudCover.Percent50,
      ImageQuality.Percent20,
      SkyBackground.Percent20,
      WaterVapor.Percent20
    )

  val Default: Constraints =
    Worst // Taken from ODB

  implicit val equalConstraints: Eq[Constraints] =
    Eq.by(x => (x.cc, x.iq, x.sb, x.wv))

  implicit val showConstraints: Show[Constraints] =
    Show.show(x => List(x.cc, x.iq, x.sb, x.wv).mkString(", "))

}
