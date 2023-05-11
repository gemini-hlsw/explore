// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import explore.events.ItcMessage
import explore.model.BasicConfigAndItc
import explore.model.TargetList
import explore.model.WorkerClients.ItcClient
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcChartResult
import explore.model.itc.ItcQueryProblems
import explore.model.itc.ItcResult
import explore.model.itc.ItcTarget
import explore.model.itc.OverridenExposureTime
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.client.OptimizedChartResult
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import queries.schemas.itc.syntax.*
import queries.schemas.odb.ObsQueries.*
import react.common.ReactFnProps
import workers.WorkerClient

case class ItcPanelProps(
  observingMode:            Option[ObservingMode],
  spectroscopyRequirements: Option[SpectroscopyRequirementsData],
  val scienceData:          Option[ScienceData],
  exposure:                 Option[ItcChartExposureTime],
  selectedConfig:           Option[BasicConfigAndItc], // selected row in spectroscopy modes table
  allTargets:               TargetList
) derives Eq:
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

  val finalSelectedConfig =
    (finalConfig, finalExposure).mapN((c, t) =>
      BasicConfigAndItc(c, ItcResult.Result(t.time, PosInt.unsafeFrom(t.count.value)).asRight.some)
    )

  val signalToNoiseAt: Option[Wavelength] = spectroscopyRequirements.flatMap(_.signalToNoiseAt)

  val wavelength: Option[CentralWavelength] = finalConfig.map {
    case c: BasicConfiguration.GmosNorthLongSlit => c.centralWavelength
    case c: BasicConfiguration.GmosSouthLongSlit => c.centralWavelength
  }

  // TODO: Revisit when we have exposure mode in spectroscopy requirements
  val signalToNoise: Option[SignalToNoise] = spectroscopyRequirements.flatMap(_.signalToNoise)

  val instrumentRow: Option[InstrumentRow] = finalConfig.map {
    case c: BasicConfiguration.GmosNorthLongSlit =>
      GmosNorthSpectroscopyRow(c.grating, c.fpu, c.filter)
    case c: BasicConfiguration.GmosSouthLongSlit =>
      GmosSouthSpectroscopyRow(c.grating, c.fpu, c.filter)
  }

  // TODO: Revisit when we have exposure mode in science requirements
  val chartExposureTime: Option[ItcChartExposureTime] = finalExposure

  val itcTargets: Option[NonEmptyList[ItcTarget]] =
    scienceData.flatMap(_.itcTargets(allTargets).toNel)

  val targets: List[ItcTarget] = itcTargets.foldMap(_.toList)

  private val queryProps =
    (observingMode,
     wavelength,
     scienceData.map(_.constraints),
     itcTargets,
     instrumentRow,
     chartExposureTime
    )

  val defaultSelectedTarget: Option[ItcTarget] =
    val r = for
      w <- wavelength
      s <- scienceData
      t  = s.itcTargets(allTargets)
      b <- t.brightestAt(w.value)
    yield b
    r.orElse(scienceData.flatMap(_.itcTargets(allTargets).headOption))

  val isExecutable: Boolean = queryProps.forall(_.isDefined)

  def requestITCData(
    onComplete:  Either[ItcQueryProblems, ItcChartResult] => IO[Unit],
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
          .requestSingle(ItcMessage.GraphQuery(w, ex.time, exposures, constraints, t, mode))
          .flatMap(
            _.fold(
              onComplete(ItcQueryProblems.GenericError("No response from ITC server").asLeft)
            )(onComplete)
          )
    action.getOrElse(orElse)

object ItcPanelProps:
  given Reusability[ItcPanelProps] = Reusability.byEq
