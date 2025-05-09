// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.actions

import cats.Endo
import cats.effect.IO
import cats.syntax.all.*
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ProgramSummaries
import explore.model.syntax.all.*
import explore.services.OdbObservationApi
import explore.undo.*
import lucuma.core.model.TimingWindow

import scala.collection.immutable.SortedSet

object ObservationPasteIntoSchedulingGroupAction:
  private def updateExpandedSchedulingGroups(
    obsList:          List[Observation.Id],
    programSummaries: ProgramSummaries,
    adding:           Boolean
  )(
    expandedIds:      SortedSet[ObsIdSet]
  ) =
    val obsWithSchedulingGroups: List[(Observation.Id, List[TimingWindow])] =
      obsList.flatMap: obsId =>
        programSummaries.observations
          .get(obsId)
          .map: obs =>
            (obsId, obs.timingWindows)

    // We'll just expand any affected asterisms
    val newGroups: List[(List[TimingWindow], List[Observation.Id])] =
      obsWithSchedulingGroups.groupMap(_._2)(_._1).toList

    newGroups.foldLeft(expandedIds) { case (eids, (sg, obsIds)) =>
      // this is safe because it was created by groupMap
      val newObsIdSet: ObsIdSet = ObsIdSet.fromList(obsIds).get

      programSummaries.schedulingGroups
        .findWithSchedulingGroup(sg)
        .fold(
          if (adding) eids + newObsIdSet else eids
        ): grp =>
          if (adding) eids + (grp.obsIds ++ newObsIdSet)
          else grp.obsIds.remove(newObsIdSet).fold(eids)(eids + _)
    }

  def apply(
    ids:            List[(Observation.Id, List[TimingWindow])],
    modExpandedIds: Endo[SortedSet[ObsIdSet]] => IO[Unit]
  )(using
    odbApi:         OdbObservationApi[IO]
  ): AsyncAction[ProgramSummaries, List[Observation.Id], Option[List[Observation]]] =
    AsyncAction(
      asyncGet = ids
        .traverse: (obsId, timingWindows) =>
          odbApi.applyObservation(obsId, onTimingWindows = timingWindows.some)
        .map(obsList => (obsList.map(_.id), obsList.some)),
      getter = obsListGetter,
      setter = obsListSetter,
      onSet = newObsIds =>
        (programSummaries, _) =>
          modExpandedIds(updateExpandedSchedulingGroups(newObsIds, programSummaries, true)),
      onRestore = newObsIds =>
        (programSummaries, olObsSumm) =>
          olObsSumm.fold(
            modExpandedIds(updateExpandedSchedulingGroups(newObsIds, programSummaries, false)) >>
              odbApi.deleteObservations(newObsIds)
          )(_ =>
            modExpandedIds(updateExpandedSchedulingGroups(newObsIds, programSummaries, true)) >>
              odbApi.undeleteObservations(newObsIds)
          )
    )
