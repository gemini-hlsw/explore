// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import explore.model.InstrumentConfigAndItcResult
import lucuma.core.enums.ScienceMode
import lucuma.schemas.model.BasicConfiguration

// ModeSelection is a collection of InstrumentConfigAndItcResult objects that are consistent with each other
final case class ConfigSelection private (configs: List[InstrumentConfigAndItcResult]) derives Eq:
  lazy val headOption: Option[InstrumentConfigAndItcResult] = configs.headOption
  lazy val count: Int                                       = configs.length
  lazy val isEmpty: Boolean                                 = configs.isEmpty
  lazy val nonEmpty: Boolean                                = configs.nonEmpty

  // We can create a configuration if there is at least one selection and all ITCs are successful.
  lazy val canAccept: Boolean =
    nonEmpty &&
      configs.forall(_.itcResult.flatMap(_.toOption).exists(_.isSuccess))

  lazy val hasItcErrors: Boolean =
    configs.exists(_.itcResult.exists(_.isLeft))

  lazy val isMissingSomeItc: Boolean =
    configs.exists(_.itcResult.isEmpty)

  lazy val hasPendingItc: Boolean =
    configs.exists(_.itcResult.exists(_.toOption.exists(_.isPending)))

  def contains(config: ItcInstrumentConfig): Boolean = configs.exists(_.instrumentConfig === config)

  def canAdd(configAndResult: InstrumentConfigAndItcResult): Boolean =
    headOption.fold(true)(head =>
      head.instrumentConfig.mode === ScienceMode.Imaging &&
        head.instrumentConfig.instrument === configAndResult.instrument &&
        !contains(configAndResult.instrumentConfig)
    )

  def add(configAndResult: InstrumentConfigAndItcResult): ConfigSelection =
    // should I throw an exception for instrument mismatch, or just ignore the new config?
    if canAdd(configAndResult) then ConfigSelection(configs :+ configAndResult)
    else this

  def remove(config: ItcInstrumentConfig): ConfigSelection =
    ConfigSelection.fromList(configs.filterNot(_.instrumentConfig === config))

  // The cmd-click handler for imaging. Toggles the inclusion of the config in the selection.
  def toggle(configAndResult: InstrumentConfigAndItcResult): ConfigSelection =
    if configs.contains_(configAndResult) then remove(configAndResult.instrumentConfig)
    else add(configAndResult)

  // Click handler for spectroscopy and non-cmd click handler for imaging.
  // If the config is in the selection, "unset" it (return None), otherwise make it the selection.
  def toggleOrSet(configAndResult: InstrumentConfigAndItcResult): ConfigSelection =
    if configs.contains_(configAndResult) then ConfigSelection.Empty
    else ConfigSelection.one(configAndResult)

  def toBasicConfiguration(withFallbackWavelength: Boolean = false): Option[BasicConfiguration] =
    configs.headOption.flatMap(_.instrumentConfig match
      case ItcInstrumentConfig.GmosNorthSpectroscopy(grating, fpu, filter, Some(cw, _, _)) =>
        BasicConfiguration.GmosNorthLongSlit(grating, filter, fpu, cw).some
      case ItcInstrumentConfig.GmosNorthSpectroscopy(grating, fpu, filter, None)
          if withFallbackWavelength =>
        BasicConfiguration
          .GmosNorthLongSlit(grating, filter, fpu, ItcInstrumentConfig.GmosFallbackCW)
          .some
      case ItcInstrumentConfig.GmosSouthSpectroscopy(grating, fpu, filter, Some(cw, _, _)) =>
        BasicConfiguration.GmosSouthLongSlit(grating, filter, fpu, cw).some
      case ItcInstrumentConfig.GmosSouthSpectroscopy(grating, fpu, filter, None)
          if withFallbackWavelength =>
        BasicConfiguration
          .GmosSouthLongSlit(grating, filter, fpu, ItcInstrumentConfig.GmosFallbackCW)
          .some
      case ItcInstrumentConfig.Flamingos2Spectroscopy(disperser, filter, fpu)              =>
        BasicConfiguration.Flamingos2LongSlit(disperser, filter, fpu).some
      case ItcInstrumentConfig.GmosNorthImaging(f, _)                                      =>
        // FIXME
        BasicConfiguration.GmosNorthImaging(NonEmptyList.of(f)).some
      case ItcInstrumentConfig.GmosSouthImaging(f, _)                                      =>
        // FIXME
        BasicConfiguration.GmosSouthImaging(NonEmptyList.of(f)).some
      case _                                                                               => none
    )

object ConfigSelection:
  val Empty: ConfigSelection = ConfigSelection(Nil)

  def one(config: InstrumentConfigAndItcResult): ConfigSelection =
    ConfigSelection(List(config))

  // ignores duplicates and configs inconsistent with the first one. But, maybe it should throw an exception?
  def fromList(configs: List[InstrumentConfigAndItcResult]): ConfigSelection =
    configs match
      case Nil    => ConfigSelection(Nil)
      case h :: t =>
        val head = one(h)
        t.foldLeft(head)(_.add(_))

  // A list of instrument configs, with no itc results.
  // Like fromList, ingnore inconsistencies and duplicates.
  def fromInstrumentConfigs(configs: List[ItcInstrumentConfig]): ConfigSelection =
    ConfigSelection.fromList(configs.map(InstrumentConfigAndItcResult(_, None)))
