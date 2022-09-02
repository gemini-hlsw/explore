// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.common.ObsQueries.*
import explore.model.ScienceMode
import explore.model.ScienceModeAdvanced
import explore.model.ScienceModeBasic
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcTarget
import explore.model.itc.OverridenExposureTime
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import queries.schemas.itc.implicits.*
import react.common.ReactFnProps

trait ItcPanelProps(
  scienceMode:              Option[ScienceMode],
  spectroscopyRequirements: Option[SpectroscopyRequirementsData],
  scienceData:              Option[ScienceData],
  exposure:                 Option[ItcChartExposureTime]
) {
  def wavelength: Option[Wavelength] = scienceMode match
    case Some(ScienceMode.GmosNorthLongSlit(_, adv)) =>
      adv.overrideWavelength.orElse(spectroscopyRequirements.flatMap(_.wavelength))

    case Some(ScienceMode.GmosSouthLongSlit(_, adv)) =>
      adv.overrideWavelength.orElse(spectroscopyRequirements.flatMap(_.wavelength))

    case _ => none

  def signalToNoise: Option[PosBigDecimal] = scienceMode match
    case Some(ScienceMode.GmosNorthLongSlit(_, adv)) =>
      ScienceModeAdvanced.GmosNorthLongSlit.overrideExposureTimeMode.some
        .andThen(
          ExposureTimeMode.signalToNoiseValue
        )
        .getOption(adv)
        .orElse(spectroscopyRequirements.flatMap(_.signalToNoise))

    case Some(ScienceMode.GmosSouthLongSlit(_, adv)) =>
      ScienceModeAdvanced.GmosSouthLongSlit.overrideExposureTimeMode.some
        .andThen(
          ExposureTimeMode.signalToNoiseValue
        )
        .getOption(adv)
        .orElse(spectroscopyRequirements.flatMap(_.signalToNoise))

    case _ =>
      spectroscopyRequirements.flatMap(_.signalToNoise)

  def instrumentRow: Option[InstrumentRow] = scienceMode match
    case Some(ScienceMode.GmosNorthLongSlit(basic, adv)) =>
      val grating = adv.overrideGrating.getOrElse(basic.grating)
      val filter  = adv.overrideFilter.orElse(basic.filter)
      val fpu     = adv.overrideFpu.getOrElse(basic.fpu)
      GmosNorthSpectroscopyRow(grating, fpu, filter).some

    case Some(ScienceMode.GmosSouthLongSlit(basic, adv)) =>
      val grating = adv.overrideGrating.getOrElse(basic.grating)
      val filter  = adv.overrideFilter.orElse(basic.filter)
      val fpu     = adv.overrideFpu.getOrElse(basic.fpu)
      GmosSouthSpectroscopyRow(grating, fpu, filter).some

    case _ =>
      none

  def chartExposureTime: Option[ItcChartExposureTime] = scienceMode match
    case Some(ScienceMode.GmosNorthLongSlit(basic, adv)) =>
      ScienceModeAdvanced.GmosNorthLongSlit.overrideExposureTimeMode.some
        .andThen(
          ExposureTimeMode.fixedExposure
        )
        .getOption(adv)
        .map(r => ItcChartExposureTime(OverridenExposureTime.FromItc, r.time, r.count))
        .orElse(
          exposure.map(r => ItcChartExposureTime(OverridenExposureTime.Overriden, r.time, r.count))
        )

    case Some(ScienceMode.GmosSouthLongSlit(_, adv)) =>
      ScienceModeAdvanced.GmosSouthLongSlit.overrideExposureTimeMode.some
        .andThen(
          ExposureTimeMode.fixedExposure
        )
        .getOption(adv)
        .map(r => ItcChartExposureTime(OverridenExposureTime.FromItc, r.time, r.count))
        .orElse(
          exposure.map(r => ItcChartExposureTime(OverridenExposureTime.Overriden, r.time, r.count))
        )

    case _ =>
      exposure

  def itcTargets: Option[NonEmptyList[ItcTarget]] = scienceData.flatMap(_.itcTargets.toNel)

  def targets: List[ItcTarget] = itcTargets.foldMap(_.toList)
}
