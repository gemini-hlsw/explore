// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import boopickle.Default.eitherPickler
import boopickle.Default.mapPickler
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
import explore.model.AsterismIds
import explore.model.BasicConfigAndItc
import explore.model.ObsSummary
import explore.model.ScienceRequirements
import explore.model.TargetList
import explore.model.WorkerClients.ItcClient
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.ItcChartResult
import explore.model.itc.ItcExposureTime
import explore.model.itc.ItcQueryProblems
import explore.model.itc.ItcResult
import explore.model.itc.ItcTarget
import explore.model.itc.OverridenExposureTime
import explore.model.reusability.given
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessValue
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Units
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.client.OptimizedChartResult
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import queries.schemas.itc.syntax.*
import queries.schemas.odb.ObsQueries.*
import react.common.ReactFnProps
import workers.WorkerClient

case class ItcProps(
  obsSummary:         ObsSummary,
  remoteExposureTime: Option[ItcExposureTime],   // time provided by the db
  selectedConfig:     Option[BasicConfigAndItc], // selected row in spectroscopy modes table
  allTargets:         TargetList
) derives Eq:
  private val spectroscopyRequirements: Option[ScienceRequirements.Spectroscopy] =
    ScienceRequirements.spectroscopy.getOption(obsSummary.scienceRequirements)

  private val observingMode = obsSummary.observingMode
  private val constraints   = obsSummary.constraints
  private val asterismIds   = obsSummary.scienceTargetIds

  // The remote configuration is read in a different query than the itc results
  // This will work even in the case the user has overriden some parameters
  private val remoteConfig = obsSummary.observingMode.map { o =>
    BasicConfigAndItc(o.toBasicConfiguration,
                      remoteExposureTime.map(ItcResult.fromItcExposureTime(_).rightNec)
    )
  }

  // The user may select a configuration on the modes tables, we'd prefer than but if not
  // we can try with the remote confiiguration provided by the database
  val finalConfig: Option[BasicConfigAndItc] =
    selectedConfig.orElse(remoteConfig)

  val signalToNoiseAt: Option[Wavelength] =
    spectroscopyRequirements.flatMap(_.signalToNoiseAt)

  private val wavelength: Option[CentralWavelength] = finalConfig.map {
    case BasicConfigAndItc(c: BasicConfiguration.GmosNorthLongSlit, _) => c.centralWavelength
    case BasicConfigAndItc(c: BasicConfiguration.GmosSouthLongSlit, _) => c.centralWavelength
  }

  // TODO: Revisit when we have exposure mode in spectroscopy requirements
  private val signalToNoise: Option[SignalToNoise] =
    spectroscopyRequirements.flatMap(_.signalToNoise)

  private val instrumentRow: Option[InstrumentRow] = finalConfig.map {
    case BasicConfigAndItc(c: BasicConfiguration.GmosNorthLongSlit, _) =>
      GmosNorthSpectroscopyRow(c.grating, c.fpu, c.filter)
    case BasicConfigAndItc(c: BasicConfiguration.GmosSouthLongSlit, _) =>
      GmosSouthSpectroscopyRow(c.grating, c.fpu, c.filter)
  }

  val itcTargets: Option[NonEmptyList[ItcTarget]] =
    asterismIds.itcTargets(allTargets).toNel

  val targets: List[ItcTarget] = itcTargets.foldMap(_.toList)

  private val queryProps =
    (observingMode, finalConfig, wavelength, constraints, itcTargets, instrumentRow, signalToNoise)

  val isExecutable: Boolean = queryProps.forall(_.isDefined)

  def targetBrightness(target: ItcTarget): Option[(Band, BrightnessValue, Units)] =
    for
      w <- wavelength
      t <- itcTargets.flatMap(_.find(_ === target))
      b <- t.brightnessNearestTo(w.value)
    yield b

  val defaultSelectedTarget: Option[ItcTarget] =
    val t = asterismIds.itcTargets(allTargets)
    val r =
      for
        w <- wavelength
        b <- t.brightestAt(w.value)
      yield b
    r.orElse(t.headOption)

  def requestITCData(
    onComplete:  Map[ItcTarget, Either[ItcQueryProblems, ItcChartResult]] => IO[Unit],
    orElse:      IO[Unit],
    beforeStart: IO[Unit]
  )(using WorkerClient[IO, ItcMessage.Request]): IO[Unit] =
    val action: Option[IO[Unit]] =
      for
        w    <- wavelength
        sn   <- signalToNoise
        t    <- itcTargets
        mode <- instrumentRow
      yield beforeStart *>
        ItcClient[IO]
          .requestSingle(
            ItcMessage.GraphQuery(w, sn, signalToNoiseAt, constraints, t, mode)
          )
          .flatMap(
            _.fold(
              onComplete(
                targets
                  .map(_ -> ItcQueryProblems.GenericError("No response from ITC server").asLeft)
                  .toMap
              )
            )(onComplete)
          )
    action.getOrElse(orElse)

object ItcProps:
  given Reusability[ItcProps] = Reusability.byEq
