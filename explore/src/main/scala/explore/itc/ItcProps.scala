// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import boopickle.Default.eitherPickler
import boopickle.Default.mapPickler
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
import explore.model.TargetList
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
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Units
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.brightestProfileAt
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import lucuma.ui.reusability.given
import queries.schemas.itc.syntax.*
import workers.WorkerClient

import scala.collection.immutable.SortedMap

case class ItcProps(
  observation:        Observation,
  remoteExposureTime: Option[ItcExposureTime],   // time provided by the db
  selectedConfig:     Option[BasicConfigAndItc], // selected row in spectroscopy modes table
  at:                 TargetList,
  modeOverrides:      Option[InstrumentOverrides]
) derives Eq:
  private val spectroscopyRequirements: Option[ScienceRequirements.Spectroscopy] =
    ScienceRequirements.spectroscopy.getOption(observation.scienceRequirements)

  private val allTargets: TargetList =
    SortedMap.from(
      at.view.mapValues(Target.sourceProfile.modify(_.gaiaFree))
    )

  private val constraints = observation.constraints
  private val asterismIds = observation.scienceTargetIds

  // The remote configuration is read in a different query than the itc results
  // This will work even in the case the user has overriden some parameters
  private val remoteConfig = observation.observingMode.map { o =>
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

  private val instrumentRow: Option[InstrumentRow] = finalConfig.map {
    case BasicConfigAndItc(c: BasicConfiguration.GmosNorthLongSlit, _) =>
      val gmosOverride: Option[GmosSpectroscopyOverrides] = modeOverrides match {
        case Some(o: GmosSpectroscopyOverrides) => o.some
        case _                                  => none
      }
      GmosNorthSpectroscopyRow(c.grating, c.fpu, c.filter, gmosOverride)
    case BasicConfigAndItc(c: BasicConfiguration.GmosSouthLongSlit, _) =>
      val gmosOverride: Option[GmosSpectroscopyOverrides] = modeOverrides match {
        case Some(o: GmosSpectroscopyOverrides) => o.some
        case _                                  => none
      }
      GmosSouthSpectroscopyRow(c.grating, c.fpu, c.filter, gmosOverride)
  }

  val itcTargets: Option[NonEmptyList[ItcTarget]] =
    asterismIds.itcTargets(allTargets).filter(_.canQueryITC).toNel

  val targets: List[ItcTarget] = itcTargets.foldMap(_.toList)

  private val queryProps: List[Option[?]] =
    List(itcTargets, finalConfig, wavelength, instrumentRow, remoteExposureTime)

  val isExecutable: Boolean = queryProps.forall(_.isDefined)

  def targetBrightness(target: ItcTarget): Option[(Band, BrightnessValue, Units)] =
    for
      w <- wavelength
      t <- itcTargets.flatMap(_.find(_ === target))
      b <- t.profile.nearestBand(w.value)
    yield b

  val defaultSelectedTarget: Option[ItcTarget] =
    val t = asterismIds.itcTargets(allTargets)
    val r =
      for
        w <- wavelength
        b <- t.brightestProfileAt(_.profile)(w.value)
      yield b
    r.orElse(t.headOption)

  def requestChart(
    onComplete:  Map[ItcTarget, Either[ItcQueryProblems, ItcChartResult]] => IO[Unit],
    orElse:      IO[Unit],
    beforeStart: IO[Unit]
  )(using WorkerClient[IO, ItcMessage.Request]): IO[Unit] =
    val action: Option[IO[Unit]] =
      for
        w     <- wavelength
        exp   <- remoteExposureTime.map(_.time)
        count <- remoteExposureTime.map(_.count)
        snAt  <- signalToNoiseAt
        t     <- itcTargets
        mode  <- instrumentRow
      yield beforeStart *>
        ItcClient[IO]
          .requestSingle(
            ItcMessage.GraphQuery(w, exp, count, snAt, constraints, t, mode)
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
  given Reusability[ItcProps] = Reusability.by(p =>
    (p.observation.scienceTargetIds.toList,
     p.observation.constraints,
     p.observation.scienceRequirements,
     p.observation.observingMode,
     p.observation.wavelength,
     p.remoteExposureTime,
     p.selectedConfig,
     p.at,
     p.modeOverrides
    )
  )
