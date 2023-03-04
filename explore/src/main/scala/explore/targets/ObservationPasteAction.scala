// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import clue.data.syntax.*
import crystal.react.View
import crystal.react.implicits.*
import explore.common.AsterismQueries
import explore.common.AsterismQueries.*
import explore.model.AsterismGroup
import explore.model.ObsIdSet
import explore.model.ObsSummaryWithConstraintsAndConf
import explore.model.TargetWithObs
import explore.model.syntax.all.*
import explore.undo.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import queries.common.TargetQueriesGQL
import queries.schemas.odb.ObsQueries

import scala.collection.immutable.SortedSet

object ObservationPasteAction {
  private def obsListGetter(
    ids: List[(Observation.Id, Target.Id)]
  ): AsterismGroupsWithObs => Option[List[ObsSummaryWithConstraintsAndConf]] = agwo =>
    ids.map((obsId, _) => agwo.observations.get(obsId)).sequence

  private def obsListSetter(ids: List[(Observation.Id, Target.Id)])(
    otwol: Option[List[ObsSummaryWithConstraintsAndConf]]
  ): AsterismGroupsWithObs => AsterismGroupsWithObs = agwo =>
    otwol.fold {
      // the Option[List]] is empty, so we're deleting.
      ids.foldLeft(agwo) { case (grps, (obsId, tid)) =>
        // the target list could have been edited, so we'll look for the current list
        val targetIds = grps.observations
          .get(obsId)
          .fold(SortedSet(tid))(o => SortedSet.from(o.scienceTargetIds))
        grps.removeObsWithTargets(obsId, targetIds)
      }

    } {
      // we insert the ones we received back into the agwo
      _.foldLeft(agwo)((grps, obsSumm) => grps.insertObs(obsSumm))
    }

  private def updateExpandedIds(
    ids:         List[(Observation.Id, Target.Id)],
    agwo:        AsterismGroupsWithObs,
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
        .findWithTargetIds(SortedSet(tid))
        .fold(
          if (adding) eids + newObsIdSet
          else eids
        )(grp =>
          if (adding) eids + (grp.obsIds ++ newObsIdSet)
          else grp.obsIds.remove(newObsIdSet).fold(eids)(eids + _)
        )
    }

  def paste(
    programId:   Program.Id,
    ids:         List[(Observation.Id, Target.Id)],
    expandedIds: View[SortedSet[ObsIdSet]]
  )(using
    c:           TransactionalClient[IO, ObservationDB]
  ): Action[AsterismGroupsWithObs, Option[List[ObsSummaryWithConstraintsAndConf]]] =
    Action(getter = obsListGetter(ids), setter = obsListSetter(ids))(
      onSet = (agwo, _) => expandedIds.mod(updateExpandedIds(ids, agwo, true)).to[IO],
      onRestore = (agwo, olObsSumm) =>
        val obsIds = ids.map(_._1)
        olObsSumm.fold(
          expandedIds.mod(updateExpandedIds(ids, agwo, false)).to[IO] >>
            ObsQueries.deleteObservations[IO](programId, obsIds)
        )(_ =>
          expandedIds.mod(updateExpandedIds(ids, agwo, true)).to[IO] >>
            ObsQueries.undeleteObservations[IO](programId, obsIds)
        )
    )
}
