// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.actions

import cats.Endo
import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ProgramSummaries
import explore.model.syntax.all.*
import explore.services.OdbObservationApi
import explore.undo.*
import lucuma.core.model.Target

import scala.collection.immutable.SortedSet

object ObservationPasteIntoAsterismAction:
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
    odbApi:         OdbObservationApi[IO]
  ): AsyncAction[ProgramSummaries, List[Observation.Id], Option[List[Observation]]] =
    AsyncAction(
      asyncGet = ids
        .traverse: (obsId, targetIds) =>
          odbApi.applyObservation(obsId, onTargets = targetIds.some)
        .map(obsList => (obsList.map(_.id), obsList.some)),
      getter = obsListGetter,
      setter = obsListSetter,
      onSet = newObsIds =>
        (programSummaries, _) => modExpandedIds(updateExpanded(newObsIds, programSummaries, true)),
      onRestore = newObsIds =>
        (programSummaries, olObsSumm) =>
          olObsSumm.fold(
            modExpandedIds(updateExpanded(newObsIds, programSummaries, false)) >>
              odbApi.deleteObservations(newObsIds)
          )(_ =>
            modExpandedIds(updateExpanded(newObsIds, programSummaries, true)) >>
              odbApi.undeleteObservations(newObsIds)
          )
    )
