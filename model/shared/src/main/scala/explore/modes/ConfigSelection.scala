// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.ScienceMode
import lucuma.schemas.model.BasicConfiguration

// ModeSelection is a collection of ItcInstrumentConfig objects that are consistent with each other
final case class ConfigSelection private (configs: NonEmptyList[ItcInstrumentConfig]) derives Eq:
  def contains(config: ItcInstrumentConfig): Boolean = configs.contains_(config)

  def canAdd(config: ItcInstrumentConfig): Boolean =
    configs.head.mode === ScienceMode.Imaging &&
      configs.head.instrument === config.instrument && !contains(config)

  def add(config: ItcInstrumentConfig): ConfigSelection =
    // should I throw an exception for instrument mismatch, or just ignore the new config?
    if canAdd(config) then ConfigSelection(configs :+ config)
    else this

  def remove(config: ItcInstrumentConfig): Option[ConfigSelection] =
    NonEmptyList.fromList(configs.toList.filterNot(_ === config)).map(ConfigSelection(_))

  // The cmd-click handler for imaging. Toggles the inclusion of the config in the selection.
  def toggle(config: ItcInstrumentConfig): Option[ConfigSelection] =
    if configs.contains_(config) then remove(config)
    else add(config).some

  // Click handler for spectroscopy and non-cmd click handler for imaging.
  // If the config is in the selection, "unset" it (return None), otherwise make it the selection.
  def toggleOrSet(config: ItcInstrumentConfig): Option[ConfigSelection] =
    if configs.contains_(config) then none
    else ConfigSelection.fromItcInstrumentConfig(config).some

  def toBasicConfiguration: Option[BasicConfiguration] =
    configs.head match
      case ItcInstrumentConfig.GmosNorthSpectroscopy(grating, fpu, filter, Some(cw, _, _)) =>
        BasicConfiguration.GmosNorthLongSlit(grating, filter, fpu, cw).some
      case ItcInstrumentConfig.GmosSouthSpectroscopy(grating, fpu, filter, Some(cw, _, _)) =>
        BasicConfiguration.GmosSouthLongSlit(grating, filter, fpu, cw).some
      case ItcInstrumentConfig.Flamingos2Spectroscopy(disperser, filter, fpu)              =>
        BasicConfiguration.Flamingos2LongSlit(disperser, filter, fpu).some
      case _                                                                               => none

object ConfigSelection:
  def fromItcInstrumentConfig(config: ItcInstrumentConfig): ConfigSelection =
    ConfigSelection(NonEmptyList.of(config))

  // ignores duplicates and configs inconsistent with the first one. But, maybe it should throw an exception?
  def fromNel(configs: NonEmptyList[ItcInstrumentConfig]): ConfigSelection =
    val head = fromItcInstrumentConfig(configs.head)
    configs.tail.foldLeft(head)(_.add(_))

  def fromList(configs: List[ItcInstrumentConfig]): Option[ConfigSelection] =
    NonEmptyList.fromList(configs).map(fromNel)
