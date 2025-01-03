// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.actions

import cats.Endo
import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ProgramSummaries
import explore.model.syntax.all.*
import explore.undo.*
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import queries.schemas.odb.ObsQueries

import scala.collection.immutable.SortedSet

object ObservationPasteIntoAsterismAction:
  private def obsListGetter(
    obsList: List[(Observation.Id, List[Target.Id])]
  ): ProgramSummaries => Option[List[Observation]] = programSummaries =>
    obsList.map((obsId, _) => programSummaries.observations.get(obsId)).sequence

  private def obsListSetter(obsList: List[(Observation.Id, List[Target.Id])])(
    otwol: Option[List[Observation]]
  ): ProgramSummaries => ProgramSummaries = programSummaries =>
    otwol.fold {
      // the Option[List]] is empty, so we're deleting.
      obsList.foldLeft(programSummaries) { case (grps, (obsId, _)) => grps.removeObs(obsId) }
    } {
      // we insert the ones we received back into the programSummaries
      _.foldLeft(programSummaries)((grps, obsSumm) => grps.insertObs(obsSumm))
    }

  private def updateExpanded(
    obsList:          List[(Observation.Id, List[Target.Id])],
    programSummaries: ProgramSummaries,
    adding:           Boolean
  )(
    expandedIds:      SortedSet[ObsIdSet]
  ) =
    // We'll just expand any affected asterisms
    val newGroups: List[(List[Target.Id], List[Observation.Id])] =
      obsList.groupMap(_._2)(_._1).toList

    newGroups.foldLeft(expandedIds) { case (eids, (tid, obsIds)) =>
      // this is safe because it was created by groupMap
      val newObsIdSet: ObsIdSet = ObsIdSet.fromList(obsIds).get

      programSummaries.asterismGroups
        .findWithTargetIds(SortedSet.from(tid))
        .fold(
          if (adding) eids + newObsIdSet else eids
        ): grp =>
          if (adding) eids + (grp.obsIds ++ newObsIdSet)
          else grp.obsIds.remove(newObsIdSet).fold(eids)(eids + _)
    }

  def apply(
    ids:            List[(Observation.Id, List[Target.Id])],
    modExpandedIds: Endo[SortedSet[ObsIdSet]] => IO[Unit]
  )(using
    c:              FetchClient[IO, ObservationDB]
  ): Action[ProgramSummaries, Option[List[Observation]]] =
    Action(getter = obsListGetter(ids), setter = obsListSetter(ids))(
      onSet = (programSummaries, _) => modExpandedIds(updateExpanded(ids, programSummaries, true)),
      onRestore = (programSummaries, olObsSumm) =>
        val obsIds = ids.map(_._1)
        olObsSumm.fold(
          modExpandedIds(updateExpanded(ids, programSummaries, false)) >>
            ObsQueries.deleteObservations[IO](obsIds)
        )(_ =>
          modExpandedIds(updateExpanded(ids, programSummaries, true)) >>
            ObsQueries.undeleteObservations[IO](obsIds)
        )
    )
