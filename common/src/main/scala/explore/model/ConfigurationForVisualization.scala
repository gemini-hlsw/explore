// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.implicits.*
import explore.model.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.PosAngleConstraint
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength

// Yet another config class. This one is has the minmimal set of params to visualize the configuration
// It is a subset of ObsConfiguration such that can be built out of either the db config
// Or the minimal config from the modes table
case class ConfigurationForVisualization private (
  configuration:              BasicConfiguration,
  scienceOffsets:             Option[NonEmptyList[Offset]],
  acquisitionOffsets:         Option[NonEmptyList[Offset]],
  selectedPosAngle:           Option[Angle],
  selectedPosAngleConstraint: Option[PosAngleConstraint],
  centralWavelength:          Option[CentralWavelength]
) derives Eq:
  // Effective pos angle, either from the AGS, or the default for the conifguratio
  // TODO: Take the calculated average parallactic angle if needed
  val posAngle: Angle =
    selectedPosAngle.getOrElse(
      configuration.obsModeType.defaultPosAngleConstraint.fallbackPosAngle(none)
    )

object ConfigurationForVisualization:
  def fromObsConfiguration(
    obsConfig: ObsConfiguration
  ): Option[ConfigurationForVisualization] =
    obsConfig.configuration
      .map { c =>
        ConfigurationForVisualization(
          c,
          obsConfig.scienceOffsets,
          obsConfig.acquisitionOffsets,
          obsConfig.selectedPA.orElse(obsConfig.fallbackPA),
          obsConfig.posAngleConstraint,
          obsConfig.centralWavelength
        )
      }
      .orElse:
        obsConfig.selectedConfig
          .toBasicConfiguration(withFallbackWavelength = true)
          .map:
            fromBasicConfiguration(_, obsConfig.fallbackPA)

  def fromBasicConfiguration(
    basicConfiguration: BasicConfiguration,
    selectedPosAngle:   Option[Angle]
  ): ConfigurationForVisualization =
    ConfigurationForVisualization(
      basicConfiguration,
      None,
      None,
      selectedPosAngle,
      None,
      basicConfiguration.centralWavelength
    )
