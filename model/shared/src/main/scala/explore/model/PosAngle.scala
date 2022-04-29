// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import lucuma.core.math.Angle

sealed trait PosAngle {
  def angle: Angle
}

object PosAngle {
  case class Fixed(angle: Angle)               extends PosAngle {
    override def toString: String = s"Fixed(${angle.toDoubleDegrees})"
  }
  case class AllowFlip(angle: Angle)           extends PosAngle {
    override def toString: String = s"AllowFlip(${angle.toDoubleDegrees})"
  }
  case class AverageParallactic(angle: Angle)  extends PosAngle {
    override def toString: String = s"AverageParallactic(${angle.toDoubleDegrees})"
  }
  case class ParallacticOverride(angle: Angle) extends PosAngle {
    override def toString: String = s"ParallacticOverride(${angle.toDoubleDegrees})"
  }

  val Default: PosAngle = Fixed(Angle.Angle0)

  implicit val eqPosAngle: Eq[PosAngle] = Eq.instance {
    case (Fixed(a), Fixed(b))                             => a === b
    case (AllowFlip(a), AllowFlip(b))                     => a === b
    case (AverageParallactic(a), AverageParallactic(b))   => a === b
    case (ParallacticOverride(a), ParallacticOverride(b)) => a === b
    case _                                                => false
  }

}
