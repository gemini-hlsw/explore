// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.View
import crystal.react.implicits.*
import explore.common.AsterismQueries.*
import explore.model.ObsIdSet
import explore.model.ObsSummary
import explore.model.syntax.all.*
import explore.undo.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import queries.schemas.odb.ObsQueries

import scala.collection.immutable.SortedSet

object ObservationInsertAction {
  private def getter(obsId: Observation.Id): ProgramSummaries => Option[ObsSummary] =
    _.observations.getValue(obsId)

  private def setter(obsId: Observation.Id)(
    optObs: Option[ObsSummary]
  ): ProgramSummaries => ProgramSummaries = agwo =>
    optObs.fold {
      // we're undoing - look to see what the current targets are.
      val targets =
        agwo.observations
          .getValue(obsId)
          .fold(SortedSet.empty[Target.Id])(o => SortedSet.from(o.scienceTargetIds))
      agwo.removeObsWithTargets(obsId, targets)
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
    programId:   Program.Id,
    obsId:       Observation.Id,
    expandedIds: View[SortedSet[ObsIdSet]],
    setPage:     Option[Observation.Id] => IO[Unit],
    postMessage: String => IO[Unit]
  )(using
    FetchClient[IO, ?, ObservationDB]
  ): Action[ProgramSummaries, Option[ObsSummary]] =
    Action(getter = getter(obsId), setter = setter(obsId))(
      onSet = (agwo, optObs) =>
        expandedIds.mod(updateExpandedIds(obsId, agwo, optObs)).to[IO] >> setPage(obsId.some),
      onRestore = (agwo, optObs) =>
        expandedIds.mod(updateExpandedIds(obsId, agwo, optObs)).to[IO] >>
          optObs.fold(
            ObsQueries.deleteObservation[IO](programId, obsId) >>
              setPage(none) >>
              postMessage(s"Deleted observation $obsId")
          )(_ =>
            ObsQueries.undeleteObservation[IO](programId, obsId) >>
              setPage(obsId.some) >>
              postMessage(s"Restored observation $obsId")
          )
    )
}
