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
import explore.model.TargetList
import explore.model.WorkerClients.ItcClient
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcChartResult
import explore.model.itc.ItcTarget
import explore.model.itc.OverridenExposureTime
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import queries.schemas.itc.ITCConversions.*
import queries.schemas.odb.ObsQueries.*
import react.common.ReactFnProps
import workers.WorkerClient

trait ItcPanelProps(
  observingMode:            Option[ObservingMode],
  spectroscopyRequirements: Option[SpectroscopyRequirementsData],
  scienceData:              Option[ScienceData],
  exposure:                 Option[ItcChartExposureTime],
  selectedConfig:           Option[BasicConfigAndItc], // selected row in spectroscopy modes table
  allTargets:               TargetList
):
  // if there is an observingMode, that means a configuration has been created. If not, we'll use the
  // row selected in the spectroscopy modes table if it exists
  val (finalConfig, finalExposure) = observingMode match {
    case Some(m) => (m.toBasicConfiguration.some, exposure)
    case None    =>
      selectedConfig match {
        case Some(b) =>
          (b.configuration.some, b.itc.flatMap(_.toOption).flatMap(_.toChartExposureTime))
        case None    => (none, none)
      }
  }

  val signalToNoiseAt: Option[Wavelength] = spectroscopyRequirements.flatMap(_.signalToNoiseAt)

  val wavelength: Option[CentralWavelength] = finalConfig.map {
    case c: BasicConfiguration.GmosNorthLongSlit => c.centralWavelength
    case c: BasicConfiguration.GmosSouthLongSlit => c.centralWavelength
  }

  // TODO: Revisit when we have exposure mode in spectroscopy requirements
  val signalToNoise: Option[PosBigDecimal] = spectroscopyRequirements.flatMap(_.signalToNoise)

  // val signalToNoise: Option[PosBigDecimal] = observingMode match
  //   case Some(ObservingMode.GmosNorthLongSlit(_, adv)) =>
  //     ObservingModeAdvanced.GmosNorthLongSlit.overrideExposureTimeMode.some
  //       .andThen(
  //         ExposureTimeMode.signalToNoiseValue
  //       )
  //       .getOption(adv)
  //       .orElse(spectroscopyRequirements.flatMap(_.signalToNoise))

  //   case Some(ObservingMode.GmosSouthLongSlit(_, adv)) =>
  //     ObservingModeAdvanced.GmosSouthLongSlit.overrideExposureTimeMode.some
  //       .andThen(
  //         ExposureTimeMode.signalToNoiseValue
  //       )
  //       .getOption(adv)
  //       .orElse(spectroscopyRequirements.flatMap(_.signalToNoise))

  //   case _ =>
  //     spectroscopyRequirements.flatMap(_.signalToNoise)

  val instrumentRow: Option[InstrumentRow] = finalConfig.map {
    case c: BasicConfiguration.GmosNorthLongSlit =>
      GmosNorthSpectroscopyRow(c.grating, c.fpu, c.filter)
    case c: BasicConfiguration.GmosSouthLongSlit =>
      GmosSouthSpectroscopyRow(c.grating, c.fpu, c.filter)
  }

  // TODO: Revisit when we have exposure mode in science requirements
  val chartExposureTime: Option[ItcChartExposureTime] = finalExposure

  // val chartExposureTime: Option[ItcChartExposureTime] = observingMode match
  //   case Some(ObservingMode.GmosNorthLongSlit(basic, adv)) =>
  //     ObservingModeAdvanced.GmosNorthLongSlit.overrideExposureTimeMode.some
  //       .andThen(
  //         ExposureTimeMode.fixedExposure
  //       )
  //       .getOption(adv)
  //       .map(r => ItcChartExposureTime(OverridenExposureTime.FromItc, r.time, r.count))
  //       .orElse(
  //         exposure.map(r => ItcChartExposureTime(OverridenExposureTime.Overriden, r.time, r.count))
  //       )

  //   case Some(ObservingMode.GmosSouthLongSlit(_, adv)) =>
  //     ObservingModeAdvanced.GmosSouthLongSlit.overrideExposureTimeMode.some
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

  val itcTargets: Option[NonEmptyList[ItcTarget]] =
    scienceData.flatMap(_.itcTargets(allTargets).toNel)

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
