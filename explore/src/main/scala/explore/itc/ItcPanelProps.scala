// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.common.ObsQueries.*
import explore.events.ItcMessage
import explore.model.ScienceMode
import explore.model.ScienceModeAdvanced
import explore.model.ScienceModeBasic
import explore.model.WorkerClients.ItcClient
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.CoverageCenterWavelength
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcChartResult
import explore.model.itc.ItcTarget
import explore.model.itc.OverridenExposureTime
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import queries.schemas.itc.implicits.*
import react.common.ReactFnProps
import workers.WorkerClient

trait ItcPanelProps(
  scienceMode:              Option[ScienceMode],
  spectroscopyRequirements: Option[SpectroscopyRequirementsData],
  scienceData:              Option[ScienceData],
  exposure:                 Option[ItcChartExposureTime]
):
  // This will not match the coverage center as used in the table
  // Will be fixed in a future PR
  val coverageCenterWavelength: Option[CoverageCenterWavelength] =
    spectroscopyRequirements.flatMap(_.wavelength).map(CoverageCenterWavelength(_))

  val signalToNoiseAt: Option[Wavelength] = spectroscopyRequirements.flatMap(_.signalToNoiseAt)

  val wavelength: Option[CoverageCenterWavelength] = scienceMode match
    case Some(ScienceMode.GmosNorthLongSlit(_, adv)) =>
      adv.overrideWavelength.map(CoverageCenterWavelength(_)).orElse(coverageCenterWavelength)

    case Some(ScienceMode.GmosSouthLongSlit(_, adv)) =>
      adv.overrideWavelength.map(CoverageCenterWavelength(_)).orElse(coverageCenterWavelength)

    case _ => none

  val signalToNoise: Option[PosBigDecimal] = scienceMode match
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

  val instrumentRow: Option[InstrumentRow] = scienceMode match
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

  val chartExposureTime: Option[ItcChartExposureTime] = scienceMode match
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

  val itcTargets: Option[NonEmptyList[ItcTarget]] = scienceData.flatMap(_.itcTargets.toNel)

  val targets: List[ItcTarget] = itcTargets.foldMap(_.toList)

  val queryProps =
    (wavelength, scienceData.map(_.constraints), itcTargets, instrumentRow, chartExposureTime)

  def requestITCData(
    onComplete:  ItcChartResult => IO[Unit],
    orElse:      IO[Unit],
    beforeStart: IO[Unit]
  )(using WorkerClient[IO, ItcMessage.Request]): IO[Unit] =
    val action: Option[IO[Unit]] =
      for
        w           <- wavelength
        ex          <- chartExposureTime
        exposures   <- refineV[Positive](ex.count.value).toOption
        constraints <- scienceData.map(_.constraints)
        t           <- itcTargets
        mode        <- instrumentRow
      yield beforeStart *>
        ItcClient[IO]
          .request(
            ItcMessage.GraphQuery(w, ex.time, exposures, constraints, t, mode)
          )
          .use(_.evalMap(onComplete).compile.drain)
    action.getOrElse(orElse)
