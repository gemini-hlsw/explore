// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.actions

import cats.Endo
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ProgramSummaries
import explore.model.syntax.all.*
import explore.undo.*
import lucuma.core.model.ConstraintSet
import lucuma.schemas.ObservationDB
import queries.schemas.odb.ObsQueries

import scala.collection.immutable.SortedSet

object ObservationPasteIntoConstraintSetAction:
  private def updateExpandedAsterisms(
    obsList:          List[Observation.Id],
    programSummaries: ProgramSummaries,
    adding:           Boolean
  )(
    expandedIds:      SortedSet[ObsIdSet]
  ) =
    val obsWithConstraintSet: List[(Observation.Id, ConstraintSet)] =
      obsList.flatMap: obsId =>
        programSummaries.observations
          .get(obsId)
          .map: obs =>
            (obsId, obs.constraints)

    // We'll just expand any affected asterisms
    val newGroups: List[(ConstraintSet, List[Observation.Id])] =
      obsWithConstraintSet.groupMap(_._2)(_._1).toList

    newGroups.foldLeft(expandedIds) { case (eids, (cs, obsIds)) =>
      // this is safe because it was created by groupMap
      val newObsIdSet: ObsIdSet = ObsIdSet.fromList(obsIds).get

      programSummaries.constraintGroups
        .findWithConstraintSet(cs)
        .fold(
          if (adding) eids + newObsIdSet else eids
        ): grp =>
          if (adding) eids + (grp.obsIds ++ newObsIdSet)
          else grp.obsIds.remove(newObsIdSet).fold(eids)(eids + _)
    }

  def apply(
    ids:            List[(Observation.Id, ConstraintSet)],
    modExpandedIds: Endo[SortedSet[ObsIdSet]] => IO[Unit]
  )(using
    c:              FetchClient[IO, ObservationDB]
  ): AsyncAction[ProgramSummaries, List[Observation.Id], Option[List[Observation]]] =
    AsyncAction(
      asyncGet = ids
        .traverse: (obsId, constraintSet) =>
          ObsQueries
            .applyObservation[IO](obsId, onConstraintSet = constraintSet.some)
        .map(obsList => (obsList.map(_.id), obsList.some)),
      getter = obsListGetter,
      setter = obsListSetter,
      onSet = newObsIds =>
        (programSummaries, _) =>
          modExpandedIds(updateExpandedAsterisms(newObsIds, programSummaries, true)),
      onRestore = newObsIds =>
        (programSummaries, olObsSumm) =>
          olObsSumm.fold(
            modExpandedIds(updateExpandedAsterisms(newObsIds, programSummaries, false)) >>
              ObsQueries.deleteObservations[IO](newObsIds)
          )(_ =>
            modExpandedIds(updateExpandedAsterisms(newObsIds, programSummaries, true)) >>
              ObsQueries.undeleteObservations[IO](newObsIds)
          )
    )
