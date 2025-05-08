// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.actions

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ProgramSummaries
import explore.model.syntax.all.*
import explore.services.OdbObservationApi
import explore.undo.*
import lucuma.core.model.Target

import scala.collection.immutable.SortedSet

object ObservationInsertAction {
  private def getter(obsId: Observation.Id): ProgramSummaries => Option[Observation] =
    _.observations.get(obsId)

  private def setter(obsId: Observation.Id)(
    optObs: Option[Observation]
  ): ProgramSummaries => ProgramSummaries = agwo =>
    optObs.fold { // undo
      agwo.removeObs(obsId)
    } { // do or re-do
      agwo.insertObs
    }

  private def updateExpandedIds(
    obsId:  Observation.Id,
    agwo:   ProgramSummaries,
    optObs: Option[Observation]
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
    odbApi:      OdbObservationApi[IO]
  ): Action[ProgramSummaries, Option[Observation]] =
    Action(getter = getter(obsId), setter = setter(obsId))(
      onSet = (agwo, optObs) =>
        expandedIds.mod(updateExpandedIds(obsId, agwo, optObs)).toAsync >> setPage(obsId.some),
      onRestore = (agwo, optObs) =>
        expandedIds.mod(updateExpandedIds(obsId, agwo, optObs)).toAsync >>
          optObs.fold(
            odbApi.deleteObservation(obsId) >>
              setPage(none) >>
              postMessage(s"Deleted observation $obsId")
          )(_ =>
            odbApi.undeleteObservation(obsId) >>
              setPage(obsId.some) >>
              postMessage(s"Restored observation $obsId")
          )
    )
}
