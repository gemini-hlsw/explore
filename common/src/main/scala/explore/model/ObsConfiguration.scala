// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import explore.model.syntax.all.*
import explore.modes.ItcInstrumentConfig
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.PosAngleConstraint
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import org.typelevel.cats.time.instances.duration.*

import java.time.Duration

case class ObsConfiguration(
  configuration:      Option[BasicConfiguration],
  selectedConfig:     Option[ItcInstrumentConfig], // selected row in spectroscopy modes table
  posAngleProperties: Option[PAProperties],
  constraints:        Option[ConstraintSet],
  centralWavelength:  Option[CentralWavelength],
  scienceOffsets:     Option[NonEmptyList[Offset]],
  acquisitionOffsets: Option[NonEmptyList[Offset]],
  averagePA:          Option[AveragePABasis],
  obsDuration:        Option[Duration],
  needGuideStar:      Boolean,
  remoteGSName:       Option[NonEmptyString],
  calibrationRole:    Option[CalibrationRole]
) derives Eq:

  def posAngleConstraint: Option[PosAngleConstraint] =
    posAngleProperties.map(_.constraint)

  def agsState: Option[View[AgsState]] =
    posAngleProperties.map(_.agsState)

  def guideStarSelection: Option[View[GuideStarSelection]] =
    posAngleProperties.map(_.guideStarSelection)

  // AGS selected position
  def selectedPA: Option[Angle] =
    posAngleProperties.flatMap(_.selectedPA)

  // In case there is no guide star we still want to have a posAngle equivalent
  // To draw visualization
  def fallbackPA: Option[Angle] =
    posAngleProperties.map(_.constraint.fallbackPosAngle(averagePA.map(_.averagePA)))

  def obsModeType: Option[ObservingModeType] =
    configuration.map(_.obsModeType)

  def toVizConfig: Option[ConfigurationForVisualization] =
    configuration
      .map { c =>
        ConfigurationForVisualization(
          c,
          scienceOffsets,
          acquisitionOffsets,
          selectedPA.orElse(fallbackPA),
          posAngleConstraint
        )
      }
      .orElse:
        selectedConfig.flatMap(_.toVizConfig(fallbackPA))

object ObsConfiguration:
  def forPlainTarget(
    configuration: Option[BasicConfiguration],
    constraints:   Option[ConstraintSet],
    wavelength:    Option[CentralWavelength],
    needsAGS:      Boolean
  ): ObsConfiguration =
    ObsConfiguration(
      configuration,
      none,
      none,
      constraints,
      wavelength,
      none,
      none,
      none,
      none,
      needsAGS,
      none,
      none
    )
