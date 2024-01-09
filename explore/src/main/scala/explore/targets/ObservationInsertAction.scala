// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.*
import explore.model.ObsIdSet
import explore.model.ObsSummary
import explore.model.ProgramSummaries
import explore.model.syntax.all.*
import explore.undo.*
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import queries.schemas.odb.ObsQueries

import scala.collection.immutable.SortedSet

trait ObservationAction:
  def getter(obsId: Observation.Id): ProgramSummaries => Option[ObsSummary] =
    _.observations.getValue(obsId)

object ObservationInsertAction extends ObservationAction:

  private def setter(obsId: Observation.Id)(
    optObs: Option[ObsSummary]
  ): ProgramSummaries => ProgramSummaries = agwo =>
    optObs.fold { // undo
      agwo.removeObs(obsId)
    } { // do or re-do
      agwo.insertObs
    }

  private def updateExpandedIds(
    obsId:  Observation.Id,
    agwo:   ProgramSummaries,
    optObs: Option[ObsSummary]
  )(expandedIds: SortedSet[ObsIdSet]) =
    // We'll just expand the associated asterism.
    val setOfOne = ObsIdSet.one(obsId)
    optObs.fold(
      // we're deleting, so find in current agwo (it should be there)
      agwo.asterismGroups
        .findContainingObsIds(setOfOne)
        .fold(expandedIds)(grp =>
          // if there is anything left in the group after removing this obs, expand it
          grp.obsIds.remove(setOfOne).fold(expandedIds)(expandedIds + _)
        )
    )(obs =>
      // we're doing or re-doing, so find the current group with this asterism
      agwo.asterismGroups
        .findWithTargetIds(SortedSet.from(obs.scienceTargetIds))
        .fold(expandedIds + setOfOne)(grp => expandedIds + (grp.obsIds ++ setOfOne))
    )

  def insert(
    obsId:       Observation.Id,
    expandedIds: View[SortedSet[ObsIdSet]],
    setPage:     Option[Observation.Id] => IO[Unit],
    postMessage: String => IO[Unit]
  )(using
    FetchClient[IO, ObservationDB]
  ): Action[ProgramSummaries, Option[ObsSummary]] =
    Action(getter = getter(obsId), setter = setter(obsId))(
      onSet = (agwo, optObs) =>
        expandedIds.mod(updateExpandedIds(obsId, agwo, optObs)).toAsync >> setPage(obsId.some),
      onRestore = (agwo, optObs) =>
        expandedIds.mod(updateExpandedIds(obsId, agwo, optObs)).toAsync >>
          optObs.fold(
            ObsQueries.deleteObservation[IO](obsId) >>
              setPage(none) >>
              postMessage(s"Deleted observation $obsId")
          )(_ =>
            ObsQueries.undeleteObservation[IO](obsId) >>
              setPage(obsId.some) >>
              postMessage(s"Restored observation $obsId")
          )
    )

object ObservationDeleteAction extends ObservationAction:
  private def updateExpandedIds(
    obsId:  Observation.Id,
    agwo:   ProgramSummaries,
    optObs: Option[ObsSummary]
  )(expandedIds: SortedSet[ObsIdSet]) =
    // We'll just expand the associated asterism.
    val setOfOne = ObsIdSet.one(obsId)
    optObs.fold(
      // we're deleting, so find in current agwo (it should be there)
      agwo.asterismGroups
        .findContainingObsIds(setOfOne)
        .fold(expandedIds)(grp =>
          // if there is anything left in the group after removing this obs, expand it
          grp.obsIds.remove(setOfOne).fold(expandedIds)(expandedIds + _)
        )
    )(obs =>
      // we're doing or re-doing, so find the current group with this asterism
      agwo.asterismGroups
        .findWithTargetIds(SortedSet.from(obs.scienceTargetIds))
        .fold(expandedIds + setOfOne)(grp => expandedIds + (grp.obsIds ++ setOfOne))
    )

  private def setter(obsId: Observation.Id)(
    optObs: Option[ObsSummary]
  ): ProgramSummaries => ProgramSummaries = agwo => {
    println(s"setter $obsId $optObs")
    optObs.fold {
      // we're undoing - look to see what the current targets are.
      val targets =
        agwo.observations
          .getValue(obsId)
          .fold(SortedSet.empty[Target.Id])(o => SortedSet.from(o.scienceTargetIds))
      agwo.removeObs(obsId)
    } { // do or re-do
      o =>
        println("should instert")
        val targets =
          agwo.observations
            .getValue(obsId)
            .fold(SortedSet.empty[Target.Id])(o => SortedSet.from(o.scienceTargetIds))
        agwo.removeObs(obsId)
    }
  }

  def delete(
    obsId:       Observation.Id,
    expandedIds: View[SortedSet[ObsIdSet]],
    setPage:     Option[Observation.Id] => IO[Unit],
    postMessage: String => IO[Unit]
  )(using
    FetchClient[IO, ObservationDB]
  ): Action[ProgramSummaries, Option[ObsSummary]] =
    Action(getter = getter(obsId), setter = setter(obsId))(
      onSet = (agwo, optObs) =>
        IO.println(s"onSet $optObs ${expandedIds.get}") *>
          ObsQueries.deleteObservation[IO](obsId), // >> setPage(none),
      // expandedIds.mod(updateExpandedIds(obsId, agwo, optObs)).to[IO] >> setPage(obsId.some),
      onRestore = (agwo, optObs) =>
        IO.println(s"onRestore $optObs ${expandedIds.get}") *>
          ObsQueries.undeleteObservation[IO](obsId)
        // expandedIds.mod(updateExpandedIds(obsId, agwo, optObs)).to[IO] >>
        //   optObs.fold(
        //     ObsQueries.deleteObservation[IO](programId, obsId) >>
        //       setPage(none) >>
        //       postMessage(s"Deleted observation $obsId")
        //   )(_ =>
        //     ObsQueries.undeleteObservation[IO](programId, obsId) >>
        //       setPage(obsId.some) >>
        //       postMessage(s"Restored observation $obsId")
        //   )
    )
