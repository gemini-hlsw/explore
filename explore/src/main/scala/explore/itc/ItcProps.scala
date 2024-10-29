// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.Eq
import cats.Order.given
import cats.data.NonEmptyList
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import explore.events.ItcMessage
import explore.model.BasicConfigAndItc
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.model.WorkerClients.ItcClient
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.*
import explore.model.reusability.given
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.GmosSpectroscopyOverrides
import explore.modes.InstrumentOverrides
import explore.modes.InstrumentRow
import japgolly.scalajs.react.Reusability
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessValue
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Units
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import lucuma.ui.reusability.given
import queries.schemas.itc.syntax.*
import workers.WorkerClient

case class ItcProps(
  observation:    Observation,
  selectedConfig: Option[BasicConfigAndItc], // selected row in spectroscopy modes table
  // modeOverrides:  Option[InstrumentOverrides],
  itcTargets:     Option[NonEmptyList[ItcTarget]]
) derives Eq:
  private val spectroscopyRequirements: Option[ScienceRequirements.Spectroscopy] =
    ScienceRequirements.spectroscopy.getOption(observation.scienceRequirements)

  private val constraints = observation.constraints

  // The remote configuration is read in a different query than the itc results
  // This will work even in the case the user has overriden some parameters
  // When we use the remote configuration we don't need the exposure time.
  private val remoteConfig: Option[InstrumentRow] = observation.toModeOverride(itcTargets)
  // .observingMode.map { o =>
  //   BasicConfigAndItc(
  //     o.toBasicConfiguration,
  //     none
  //   )
  // }

  // The user may select a configuration on the modes tables, we'd prefer than but if not
  // we can try with the remote confiiguration provided by the database
  val finalConfig: Option[BasicConfigAndItc] =
    selectedConfig.orElse(remoteConfig)

  val signalToNoise: Option[SignalToNoise] =
    spectroscopyRequirements.flatMap(_.signalToNoise)

  val signalToNoiseAt: Option[Wavelength] =
    spectroscopyRequirements.flatMap(_.signalToNoiseAt)

  private val wavelength: Option[CentralWavelength] =
    finalConfig
      .map(_.configuration)
      .map:
        case BasicConfiguration.GmosNorthLongSlit(_, _, _, cw) =>
          modeOverrides match
            case Some(GmosSpectroscopyOverrides(overrideCw, _, _)) => overrideCw
            case _                                                 => cw
        case BasicConfiguration.GmosSouthLongSlit(_, _, _, cw) =>
          modeOverrides match
            case Some(GmosSpectroscopyOverrides(overrideCw, _, _)) => overrideCw
            case _                                                 => cw

  private val instrumentRow: Option[InstrumentRow] =
    finalConfig
      .map(_.configuration)
      .map:
        case BasicConfiguration.GmosNorthLongSlit(grating, filter, fpu, _) =>
          val gmosOverride: Option[GmosSpectroscopyOverrides] = modeOverrides match
            case Some(o @ GmosSpectroscopyOverrides(_, _, _)) => o.some
            case _                                            => none
          GmosNorthSpectroscopyRow(grating, fpu, filter, gmosOverride)
        case BasicConfiguration.GmosSouthLongSlit(grating, filter, fpu, _) =>
          val gmosOverride: Option[GmosSpectroscopyOverrides] = modeOverrides match
            case Some(o @ GmosSpectroscopyOverrides(_, _, _)) => o.some
            case _                                            => none
          GmosSouthSpectroscopyRow(grating, fpu, filter, gmosOverride)

  private val queryProps: List[Option[?]] =
    List(itcTargets, finalConfig, wavelength, instrumentRow, signalToNoise)

  val isExecutable: Boolean = queryProps.forall(_.isDefined)

  def targetBrightness(target: ItcTarget): Option[(Band, BrightnessValue, Units)] =
    for
      w <- wavelength
      t <- itcTargets.flatMap(_.find(_ === target))
      b <- t.sourceProfile.nearestBand(w.value)
    yield b

  // Returns graphs for each target and the brightest target
  def requestGraphs(using
    WorkerClient[IO, ItcMessage.Request]
  ): IO[ItcAsterismGraphResults] =
    val action: Option[IO[ItcAsterismGraphResults]] =
      for
        w    <- wavelength
        sn   <- signalToNoise
        snAt <- signalToNoiseAt
        t    <- itcTargets
        mode <- instrumentRow
      yield IO.println(
        s"requesting graphs; wavelength: $w, signalToNoise $sn, signalToNoiseAt $snAt, instrumentRow $mode"
      ) >>
        ItcClient[IO]
          .requestSingle:
            ItcMessage.GraphQuery(w, sn, snAt, constraints, t, mode)
          .map:
            _.toRight(new Throwable("No response from ITC server."))
          .rethrow
          .flatTap: result =>
            IO.println:
              s"result: ${result.asterismGraphs.values.map(_.toOption.map(_.itcExposureTime))}"

    action.getOrElse:
      IO.raiseError:
        new Throwable("Not enough information to calculate the ITC graph or no mode selected.")

object ItcProps:
  given Reusability[ItcProps] =
    Reusability.by: p =>
      (p.observation.scienceTargetIds.toList.sorted,
       p.observation.constraints,
       p.observation.scienceRequirements,
       p.observation.wavelength,
       p.observation.basicConfiguration,
       p.selectedConfig,
       p.itcTargets,
       p.modeOverrides
      )
