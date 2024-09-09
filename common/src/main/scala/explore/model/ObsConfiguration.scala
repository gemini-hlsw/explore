// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.enums.AgsState
import lucuma.ags.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.PosAngleConstraint
import lucuma.schemas.model.BasicConfiguration
import org.typelevel.cats.time.instances.duration.*

import java.time.Duration

case class ObsConfiguration(
  configuration:      Option[BasicConfiguration],
  posAngleProperties: Option[PAProperties],
  constraints:        Option[ConstraintSet],
  wavelength:         Option[Wavelength],
  scienceOffsets:     Option[NonEmptyList[Offset]],
  acquisitionOffsets: Option[NonEmptyList[Offset]],
  averagePA:          Option[AveragePABasis],
  obsDuration:        Option[Duration],
  needGuideStar:      Boolean,
  selectedGSName:     Option[NonEmptyString]
) derives Eq:
  // In case there is no guide star we still want to have a posAngle equivalent
  // To draw visualization
  def fallbackPosAngle: Option[Angle] =
    posAngleConstraint match
      case Some(PosAngleConstraint.Fixed(a))               => a.some
      case Some(PosAngleConstraint.AllowFlip(a))           => a.some
      case Some(PosAngleConstraint.ParallacticOverride(a)) => a.some
      case Some(PosAngleConstraint.Unbounded)              => Angle.Angle0.some
      case Some(PosAngleConstraint.AverageParallactic)     =>
        averagePA.map(_.averagePA).orElse(Angle.Angle0.some)
      case _                                               => none

  def posAngleConstraintView: Option[View[PosAngleConstraint]] =
    posAngleProperties.map(_.constraint)

  def posAngleConstraint: Option[PosAngleConstraint] = posAngleConstraintView.map(_.get)

  def agsState: Option[View[AgsState]] =
    posAngleProperties.map(_.agsState)

  // Selected guide star via ags or manual
  // def selectedGS: Option[View[Option[AgsAnalysis]]] =
  //   posAngleProperties.map(_.selectedGS)
  //
  // def selectedPA: Option[Angle] =
  //   posAngleProperties.flatMap(_.selectedPA)
