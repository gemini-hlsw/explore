// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.implicits.*
import explore.model.syntax.all.*
import explore.modes.ItcInstrumentConfig
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.PosAngleConstraint
import lucuma.schemas.model.BasicConfiguration

// Yet another config class. This one is has the minmimal set of params to visualize the configuration
// It is a subset of ObsConfiguration such that can be built out of either the db config
// Or the minimal config from the modes table
case class ConfigurationForVisualization private (
  configuration:              BasicConfiguration,
  scienceOffsets:             Option[NonEmptyList[Offset]],
  acquisitionOffsets:         Option[NonEmptyList[Offset]],
  selectedPosAngle:           Option[Angle],
  selectedPosAngleConstraint: Option[PosAngleConstraint]
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
          obsConfig.posAngleConstraint
        )
      }
      .orElse:
        obsConfig.selectedConfig.flatMap(fromItcInstrumentConfig(_, obsConfig.fallbackPA))

  def fromBasicConfiguration(
    basicConfiguration: BasicConfiguration,
    selectedPosAngle:   Option[Angle]
  ): ConfigurationForVisualization =
    ConfigurationForVisualization(
      basicConfiguration,
      None,
      None,
      selectedPosAngle,
      None
    )

  def fromItcInstrumentConfig(
    itcInstrumentConfig: ItcInstrumentConfig,
    selectedPosAngle:    Option[Angle]
  ): Option[ConfigurationForVisualization] =
    itcInstrumentConfig match {
      case c: ItcInstrumentConfig.GmosNorthSpectroscopy  =>
        // This configuration is used for aladin, and wavelength is ignored
        // Nevertheless we'll use the override if available with a fallback
        val cw = c.modeOverrides
          .map(_.centralWavelength)
          .getOrElse(ItcInstrumentConfig.GmosFallbackCW)
        fromBasicConfiguration(
          BasicConfiguration.GmosNorthLongSlit(c.grating, c.filter, c.fpu, cw),
          selectedPosAngle
        ).some
      case c: ItcInstrumentConfig.GmosSouthSpectroscopy  =>
        // This configuration is used for aladin, and wavelength is ignored
        // Nevertheless we'll use the override if available with a fallback
        val cw = c.modeOverrides
          .map(_.centralWavelength)
          .getOrElse(ItcInstrumentConfig.GmosFallbackCW)
        fromBasicConfiguration(
          BasicConfiguration.GmosSouthLongSlit(c.grating, c.filter, c.fpu, cw),
          selectedPosAngle
        ).some
      case f: ItcInstrumentConfig.Flamingos2Spectroscopy =>
        fromBasicConfiguration(
          BasicConfiguration.F2LongSlit(f.grating, f.filter, f.fpu),
          selectedPosAngle
        ).some
      case _                                             => none
    }
