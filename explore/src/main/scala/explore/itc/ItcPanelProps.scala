// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.events.ItcMessage
import explore.model.BasicConfigAndItc
import explore.model.BasicConfiguration
import explore.model.ScienceMode
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
import queries.schemas.itc.ITCConversions.*
import queries.schemas.odb.ObsQueries.*
import react.common.ReactFnProps
import workers.WorkerClient

trait ItcPanelProps(
  scienceMode:              Option[ScienceMode],
  spectroscopyRequirements: Option[SpectroscopyRequirementsData],
  scienceData:              Option[ScienceData],
  exposure:                 Option[ItcChartExposureTime],
  selectedConfig:           Option[BasicConfigAndItc] // selected row in spectroscopy modes table
):
  // if there is a scienceMode, that means a configuration has been created. If not, we'll use the
  // row selected in the spectroscopy modes table if it exists
  val configAndItc: Option[BasicConfigAndItc] =
    scienceMode.map(m => BasicConfigAndItc(m.toBasicConfiguration, exposure)).orElse(selectedConfig)

  val signalToNoiseAt: Option[Wavelength] = spectroscopyRequirements.flatMap(_.signalToNoiseAt)

  val wavelength: Option[CoverageCenterWavelength] = configAndItc match
    case Some(BasicConfigAndItc(c: BasicConfiguration.GmosNorthLongSlit, _)) =>
      c.centralWavelength.some

    case Some(BasicConfigAndItc(c: BasicConfiguration.GmosSouthLongSlit, _)) =>
      c.centralWavelength.some

    case _ => none

    // TODO: Revisit when we have exposure mode in spectroscopy requirements
  val signalToNoise: Option[PosBigDecimal]         = spectroscopyRequirements.flatMap(_.signalToNoise)

  // val signalToNoise: Option[PosBigDecimal] = scienceMode match
  //   case Some(ScienceMode.GmosNorthLongSlit(_, adv)) =>
  //     ScienceModeAdvanced.GmosNorthLongSlit.overrideExposureTimeMode.some
  //       .andThen(
  //         ExposureTimeMode.signalToNoiseValue
  //       )
  //       .getOption(adv)
  //       .orElse(spectroscopyRequirements.flatMap(_.signalToNoise))

  //   case Some(ScienceMode.GmosSouthLongSlit(_, adv)) =>
  //     ScienceModeAdvanced.GmosSouthLongSlit.overrideExposureTimeMode.some
  //       .andThen(
  //         ExposureTimeMode.signalToNoiseValue
  //       )
  //       .getOption(adv)
  //       .orElse(spectroscopyRequirements.flatMap(_.signalToNoise))

  //   case _ =>
  //     spectroscopyRequirements.flatMap(_.signalToNoise)

  val instrumentRow: Option[InstrumentRow]            = configAndItc match
    case Some(BasicConfigAndItc(c: BasicConfiguration.GmosNorthLongSlit, _)) =>
      GmosNorthSpectroscopyRow(c.grating, c.fpu, c.filter).some

    case Some(BasicConfigAndItc(c: BasicConfiguration.GmosSouthLongSlit, _)) =>
      GmosSouthSpectroscopyRow(c.grating, c.fpu, c.filter).some

    case _ =>
      none

    // TODO: Revisit when we have exposure mode in science requirements
  val chartExposureTime: Option[ItcChartExposureTime] = configAndItc.flatMap(_.exposureTime)

  // val chartExposureTime: Option[ItcChartExposureTime] = scienceMode match
  //   case Some(ScienceMode.GmosNorthLongSlit(basic, adv)) =>
  //     ScienceModeAdvanced.GmosNorthLongSlit.overrideExposureTimeMode.some
  //       .andThen(
  //         ExposureTimeMode.fixedExposure
  //       )
  //       .getOption(adv)
  //       .map(r => ItcChartExposureTime(OverridenExposureTime.FromItc, r.time, r.count))
  //       .orElse(
  //         exposure.map(r => ItcChartExposureTime(OverridenExposureTime.Overriden, r.time, r.count))
  //       )

  //   case Some(ScienceMode.GmosSouthLongSlit(_, adv)) =>
  //     ScienceModeAdvanced.GmosSouthLongSlit.overrideExposureTimeMode.some
  //       .andThen(
  //         ExposureTimeMode.fixedExposure
  //       )
  //       .getOption(adv)
  //       .map(r => ItcChartExposureTime(OverridenExposureTime.FromItc, r.time, r.count))
  //       .orElse(
  //         exposure.map(r => ItcChartExposureTime(OverridenExposureTime.Overriden, r.time, r.count))
  //       )

  //   case _ =>
  //     exposure

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
