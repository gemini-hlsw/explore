// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import eu.timepit.refined.cats._
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.PosAngleConstraint
import monocle.Focus
import org.typelevel.cats.time.instantInstances

import java.time.Instant

case class ObsConfiguration(
  vizTime:            Instant,
  scienceMode:        Option[ScienceMode],
  posAngleConstraint: Option[PosAngleConstraint],
  constraints:        Option[ConstraintSet],
  wavelength:         Option[Wavelength]
) {
  def hasPosAngleConstraint: Boolean = posAngleConstraint.isDefined

  def canSelectGuideStar: Boolean =
    hasPosAngleConstraint && scienceMode.isDefined && constraints.isDefined

  def posAngle: Option[Angle] = posAngleConstraint.collect {
    case PosAngleConstraint.Fixed(a)               => a
    case PosAngleConstraint.AllowFlip(a)           => a
    case PosAngleConstraint.ParallacticOverride(a) => a
  }
}

object ObsConfiguration {
  val vizTime            = Focus[ObsConfiguration](_.vizTime)
  val searchingTarget    = Focus[ObsConfiguration](_.scienceMode)
  val posAngleConstraint = Focus[ObsConfiguration](_.posAngleConstraint)
  val constraints        = Focus[ObsConfiguration](_.constraints)
  val wavelength         = Focus[ObsConfiguration](_.wavelength)

  implicit val eqRootModel: Eq[ObsConfiguration] =
    Eq.by(m => (m.vizTime, m.scienceMode, m.posAngleConstraint, m.constraints, m.wavelength))
}
