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
    obsList: List[Observation.Id]
  ): ProgramSummaries => Option[List[Observation]] =
    programSummaries => obsList.map(programSummaries.observations.get(_)).sequence

  private def obsListSetter(obsList: List[Observation.Id])(
    otwol: Option[List[Observation]]
  ): ProgramSummaries => ProgramSummaries =
    programSummaries =>
      otwol.fold {
        // the Option[List]] is empty, so we're deleting.
        obsList.foldLeft(programSummaries) { case (grps, obsId) => grps.removeObs(obsId) }
      } {
        // we insert the ones we received back into the programSummaries
        _.foldLeft(programSummaries)((grps, obsSumm) => grps.insertObs(obsSumm))
      }

  private def updateExpanded(
    obsList:          List[Observation.Id],
    programSummaries: ProgramSummaries,
    adding:           Boolean
  )(
    expandedIds:      SortedSet[ObsIdSet]
  ) =
    val obsWithTargetList: List[(Observation.Id, List[Target.Id])] =
      obsList.flatMap: obsId =>
        programSummaries.observations
          .get(obsId)
          .map: obs =>
            (obsId, obs.scienceTargetIds.toList)

    // We'll just expand any affected asterisms
    val newGroups: List[(List[Target.Id], List[Observation.Id])] =
      obsWithTargetList.groupMap(_._2)(_._1).toList

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
  ): AsyncAction[ProgramSummaries, List[Observation.Id], Option[List[Observation]]] =
    AsyncAction(
      asyncGet = ids
        .traverse: (obsId, targetIds) =>
          ObsQueries
            .applyObservation[IO](obsId, onTargets = targetIds.some)
        .map(obsList => (obsList.map(_.id), obsList.some)),
      getter = obsListGetter,
      setter = obsListSetter,
      onSet = newObsIds =>
        (programSummaries, _) => modExpandedIds(updateExpanded(newObsIds, programSummaries, true)),
      onRestore = newObsIds =>
        (programSummaries, olObsSumm) =>
          olObsSumm.fold(
            modExpandedIds(updateExpanded(newObsIds, programSummaries, false)) >>
              ObsQueries.deleteObservations[IO](newObsIds)
          )(_ =>
            modExpandedIds(updateExpanded(newObsIds, programSummaries, true)) >>
              ObsQueries.undeleteObservations[IO](newObsIds)
          )
    )
