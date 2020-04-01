// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import cats.implicits._
import monocle.macros.Lenses
import enum._

@Lenses
final case class Conditions(
  cc: CloudCover,
  iq: ImageQuality,
  sb: SkyBackground,
  wv: WaterVapor
)

object Conditions {

  val Worst: Conditions =
    Conditions(
      CloudCover.Any,
      ImageQuality.Any,
      SkyBackground.Any,
      WaterVapor.Any
    )

  val Nominal: Conditions =
    Conditions(
      CloudCover.Percent50,
      ImageQuality.Percent70,
      SkyBackground.Percent50,
      WaterVapor.Any
    )

  val Best: Conditions =
    Conditions(
      // In the ODB model it's 20% but that value it's marked as obsolete
      // so I took the non-obsolete lowest value.
      CloudCover.Percent50,
      ImageQuality.Percent20,
      SkyBackground.Percent20,
      WaterVapor.Percent20
    )

  val Default: Conditions =
    Worst // Taken from ODB

  implicit val equalConditions: Eq[Conditions] =
    Eq.by(x => (x.cc, x.iq, x.sb, x.wv))

  implicit val showConditions: Show[Conditions] =
    Show.show(x => List(x.cc, x.iq, x.sb, x.wv).mkString(", "))

}
