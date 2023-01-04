// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
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
) derives Eq {
  // Move to lucuma-catalog
  def positions = posAngleConstraint match
    case Some(PosAngleConstraint.Fixed(a))               => NonEmptyList.of(a).some
    case Some(PosAngleConstraint.AllowFlip(a))           => NonEmptyList.of(a, a.flip).some
    case Some(PosAngleConstraint.ParallacticOverride(a)) => NonEmptyList.of(a).some
    case None                                            => NonEmptyList.fromList(ObsConfiguration.UnconstrainedAngles)
    case _                                               => None

  def hasPosAngleConstraint: Boolean = positions.isDefined

  def canSelectGuideStar: Boolean =
    hasPosAngleConstraint && scienceMode.isDefined && constraints.isDefined

}

object ObsConfiguration:
  val vizTime            = Focus[ObsConfiguration](_.vizTime)
  val searchingTarget    = Focus[ObsConfiguration](_.scienceMode)
  val posAngleConstraint = Focus[ObsConfiguration](_.posAngleConstraint)
  val constraints        = Focus[ObsConfiguration](_.constraints)
  val wavelength         = Focus[ObsConfiguration](_.wavelength)

  val UnconstrainedAngles =
    (0 until 360 by 10).map(a => Angle.fromDoubleDegrees(a.toDouble)).toList
