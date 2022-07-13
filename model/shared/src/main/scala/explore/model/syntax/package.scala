// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.syntax

import cats.syntax.all._
import explore.model.enums.PosAngleOptions
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.model.PosAngleConstraint

object all {

  implicit class PosAngleOptionsOps(val pa: PosAngleOptions) extends AnyVal {
    def toPosAngle(a: Angle): Option[PosAngleConstraint] = pa match {
      case PosAngleOptions.Fixed               => PosAngleConstraint.Fixed(a).some
      case PosAngleOptions.AllowFlip           => PosAngleConstraint.AllowFlip(a).some
      case PosAngleOptions.AverageParallactic  => PosAngleConstraint.AverageParallactic.some
      case PosAngleOptions.ParallacticOverride => PosAngleConstraint.ParallacticOverride(a).some
      case PosAngleOptions.Unconstrained       => none
    }
  }

  implicit class PosAngleOps(val pa: Option[PosAngleConstraint]) extends AnyVal {
    def toPosAngleOptions: PosAngleOptions = pa match {
      case Some(PosAngleConstraint.Fixed(_))               => PosAngleOptions.Fixed
      case Some(PosAngleConstraint.AllowFlip(_))           => PosAngleOptions.AllowFlip
      case Some(PosAngleConstraint.AverageParallactic)     => PosAngleOptions.AverageParallactic
      case Some(PosAngleConstraint.ParallacticOverride(_)) => PosAngleOptions.ParallacticOverride
      case _                                               => PosAngleOptions.Unconstrained
    }
  }

  // TODO Move to core
  implicit class SiteOps(val site: Site) extends AnyVal {
    def inPreferredDeclination(d: Declination): Boolean =
      d.toAngle.toSignedDoubleDegrees match {
        case d if d < -40.0 => site === Site.GS
        case d if d > 30.0  => site === Site.GN
        case _              => true
      }
  }
}
