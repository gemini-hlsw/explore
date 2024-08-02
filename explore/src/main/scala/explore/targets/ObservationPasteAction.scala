// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.*
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ProgramSummaries
import explore.model.syntax.all.*
import explore.undo.*
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import queries.schemas.odb.ObsQueries

import scala.collection.immutable.SortedSet

object ObservationPasteAction {
  private def obsListGetter(
    ids: List[(Observation.Id, List[Target.Id])]
  ): ProgramSummaries => Option[List[Observation]] = agwo =>
    ids.map((obsId, _) => agwo.observations.getValue(obsId)).sequence

  private def obsListSetter(ids: List[(Observation.Id, List[Target.Id])])(
    otwol: Option[List[Observation]]
  ): ProgramSummaries => ProgramSummaries = agwo =>
    otwol.fold {
      // the Option[List]] is empty, so we're deleting.
      ids.foldLeft(agwo) { case (grps, (obsId, _)) => grps.removeObs(obsId) }

    } {
      // we insert the ones we received back into the agwo
      _.foldLeft(agwo)((grps, obsSumm) => grps.insertObs(obsSumm))
    }

  private def updateExpandedIds(
    ids:         List[(Observation.Id, List[Target.Id])],
    agwo:        ProgramSummaries,
    adding:      Boolean
  )(
    expandedIds: SortedSet[ObsIdSet]
  ) =
    // We'll just expand any affected asterisms
    val newGroups = ids.groupMap(_._2)(_._1).toList
    newGroups.foldLeft(expandedIds) { case (eids, (tid, obsIds)) =>
      // this is safe because it was created by groupMap
      val newObsIdSet = ObsIdSet.fromList(obsIds).get
      agwo.asterismGroups
        .findWithTargetIds(SortedSet.from(tid))
        .fold(
          if (adding) eids + newObsIdSet
          else eids
        )(grp =>
          if (adding) eids + (grp.obsIds ++ newObsIdSet)
          else grp.obsIds.remove(newObsIdSet).fold(eids)(eids + _)
        )
    }

  def paste(
    ids:         List[(Observation.Id, List[Target.Id])],
    expandedIds: View[SortedSet[ObsIdSet]]
  )(using
    c:           FetchClient[IO, ObservationDB]
  ): Action[ProgramSummaries, Option[List[Observation]]] =
    Action(getter = obsListGetter(ids), setter = obsListSetter(ids))(
      onSet = (agwo, _) => expandedIds.mod(updateExpandedIds(ids, agwo, true)).toAsync,
      onRestore = (agwo, olObsSumm) =>
        val obsIds = ids.map(_._1)
        olObsSumm.fold(
          expandedIds.mod(updateExpandedIds(ids, agwo, false)).toAsync >>
            ObsQueries.deleteObservations[IO](obsIds)
        )(_ =>
          expandedIds.mod(updateExpandedIds(ids, agwo, true)).toAsync >>
            ObsQueries.undeleteObservations[IO](obsIds)
        )
    )
}
