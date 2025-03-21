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
import explore.model.InstrumentConfigAndItcResult
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.model.TargetList
import explore.model.WorkerClients.ItcClient
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.*
import explore.model.reusability.given
import explore.modes.InstrumentConfig
import japgolly.scalajs.react.Reusability
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.Target
import lucuma.ui.reusability.given
import queries.schemas.itc.syntax.*
import workers.WorkerClient

import scala.collection.immutable.SortedMap

case class ItcGraphQuerier(
  observation:    Observation,
  selectedConfig: Option[InstrumentConfigAndItcResult], // selected row in spectroscopy modes table
  at:             TargetList
) derives Eq:
  private val spectroscopyRequirements: Option[ScienceRequirements.Spectroscopy] =
    ScienceRequirements.spectroscopy.getOption(observation.scienceRequirements)

  private val allTargets: TargetList =
    SortedMap.from:
      at.view.mapValues(Target.sourceProfile.modify(_.gaiaFree))

  private val constraints = observation.constraints
  private val asterismIds = observation.scienceTargetIds

  // The remote configuration is read in a different query than the itc results.
  // This will work even in the case the user has overriden some parameters.
  // When we use the remote configuration we don't need the exposure time.
  private val remoteConfig: Option[InstrumentConfigAndItcResult] =
    observation
      .toInstrumentConfig(at)
      .map: row =>
        InstrumentConfigAndItcResult(row, none)

  // If the observation has an assigned configuration, we use that one.
  // Otherwise, we use the one selected in the table.
  val finalConfig: Option[InstrumentConfigAndItcResult] =
    remoteConfig.orElse(selectedConfig)

  spectroscopyRequirements.map(_.extractSignalToNoise)

  val signalToNoise: Option[SignalToNoise] =
    spectroscopyRequirements.flatMap(_.extractSignalToNoise._1)

  val signalToNoiseAt: Option[Wavelength] =
    spectroscopyRequirements.flatMap(_.extractSignalToNoise._2)

  private val instrumentConfig: Option[InstrumentConfig] =
    finalConfig.map(_.instrumentConfig)

  val itcTargets: Option[NonEmptyList[ItcTarget]] =
    asterismIds.itcTargets(allTargets).filter(_.canQueryITC).toNel

  val targets: List[ItcTarget] = itcTargets.foldMap(_.toList)

  private val queryProps =
    (signalToNoise, signalToNoiseAt, constraints.some, itcTargets, instrumentConfig).tupled

  val isExecutable: Boolean = queryProps.isDefined

  // Returns graphs for each target and the brightest target
  def requestGraphs(using
    WorkerClient[IO, ItcMessage.Request]
  ): IO[ItcAsterismGraphResults] =
    val action: Option[IO[ItcAsterismGraphResults]] =
      queryProps.map: (sn, snAt, c, t, mode) =>
        ItcClient[IO]
          .requestSingle:
            ItcMessage.GraphQuery(snAt, sn, c, t, mode)
          .map:
            _.toRight(new Throwable("No response from ITC server."))
          .rethrow

    action.getOrElse:
      IO.raiseError:
        val tgtMsg = asterismIds.toList match
          case Nil         => new Throwable("no targets.")
          case head :: Nil => s"target: $head"
          case tgts        => s"""targets: ${tgts.mkString(", ")}"""
        new Throwable(s"Could not generate a graph for $tgtMsg")

object ItcGraphQuerier:
  given Reusability[ItcGraphQuerier] =
    Reusability.by(_.queryProps)
