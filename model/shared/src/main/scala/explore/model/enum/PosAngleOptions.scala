// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

sealed trait PosAngleOptions extends Product with Serializable {
  def tag: NonEmptyString
  def longName: NonEmptyString = tag
}

object PosAngleOptions {
  case object Fixed extends PosAngleOptions {
    override val tag = "Fixed"
  }

  case object AllowFlip extends PosAngleOptions {
    override val tag      = "AllowFlip"
    override val longName = "Allow 180° flip"
  }

  case object AverageParallactic extends PosAngleOptions {
    override val tag      = "AverageParallactic"
    override val longName = "Average Parallactic"
  }

  case object ParallacticOverride extends PosAngleOptions {
    override val tag      = "ParallacticOverride"
    override val longName = "Parallactic Override"
  }

  case object Unconstrained extends PosAngleOptions {
    override val tag = "Unconstrained"
  }

  implicit val posAngleOptionsEnumeration: Enumerated[PosAngleOptions] =
    Enumerated.of(Fixed, AllowFlip, AverageParallactic, ParallacticOverride, Unconstrained)

  implicit val posAngleOptionsDisplay: Display[PosAngleOptions] =
    Display.by(_.longName.value, _.longName.value)

}
