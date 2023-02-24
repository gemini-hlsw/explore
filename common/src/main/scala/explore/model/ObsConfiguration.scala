// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import lucuma.ags.*
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.PosAngleConstraint
import lucuma.schemas.model.BasicConfiguration
import monocle.Focus
import org.typelevel.cats.time.instantInstances

import java.time.Instant

case class ObsConfiguration(
  vizTime:            Instant,
  configuration:      Option[BasicConfiguration],
  posAngleConstraint: Option[PosAngleConstraint],
  constraints:        Option[ConstraintSet],
  wavelength:         Option[Wavelength]
) derives Eq

object ObsConfiguration:
  val vizTime            = Focus[ObsConfiguration](_.vizTime)
  val configuration      = Focus[ObsConfiguration](_.configuration)
  val posAngleConstraint = Focus[ObsConfiguration](_.posAngleConstraint)
  val constraints        = Focus[ObsConfiguration](_.constraints)
  val wavelength         = Focus[ObsConfiguration](_.wavelength)
