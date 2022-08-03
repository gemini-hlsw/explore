// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.refined._

sealed trait PosAngleOptions extends Product with Serializable {
  def tag: NonEmptyString
  def longName: NonEmptyString = tag
}

object PosAngleOptions {
  case object Fixed extends PosAngleOptions {
    override val tag = "Fixed".refined
  }

  case object AllowFlip extends PosAngleOptions {
    override val tag      = "AllowFlip".refined
    override val longName = "Allow 180° flip".refined
  }

  case object AverageParallactic extends PosAngleOptions {
    override val tag      = "AverageParallactic".refined
    override val longName = "Average Parallactic".refined
  }

  case object ParallacticOverride extends PosAngleOptions {
    override val tag      = "ParallacticOverride".refined
    override val longName = "Parallactic Override".refined
  }

  case object Unconstrained extends PosAngleOptions {
    override val tag = "Unconstrained".refined
  }

  implicit val posAngleOptionsEnumeration: Enumerated[PosAngleOptions] =
    Enumerated.of(Fixed, AllowFlip, AverageParallactic, ParallacticOverride, Unconstrained)

  implicit val posAngleOptionsDisplay: Display[PosAngleOptions] =
    Display.by(_.longName.value, _.longName.value)

}
