// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import crystal.react.View
import crystal.react.implicits.*
import explore.common.AsterismQueries
import explore.common.AsterismQueries.*
import explore.data.KeyedIndexedList
import explore.model.AsterismGroup
import explore.model.ObsIdSet
import explore.model.ObsSummary
import explore.model.ObservationList
import explore.model.TargetList
import explore.model.TargetWithObs
import explore.model.syntax.all.*
import explore.undo.*
import japgolly.scalajs.react.callback.Callback
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import monocle.Iso
import queries.common.TargetQueriesGQL

import scala.annotation.unused
import scala.collection.immutable.SortedSet

object AsterismGroupObsListActions {
  private def updateExpandedIds(
    draggedIds: ObsIdSet,
    optDestIds: Option[ObsIdSet]
  )(
    eids:       SortedSet[ObsIdSet]
  ) =
    optDestIds.fold(
      eids.flatMap(ids => ids.remove(draggedIds)) + draggedIds
    ) { destIds =>
      eids.flatMap(ids =>
        if (ids === destIds) none
        else ids.remove(draggedIds)
      ) + (destIds ++ draggedIds)
    }

  def dropObservations(
    programId:   Program.Id,
    draggedIds:  ObsIdSet,
    srcIds:      ObsIdSet,
    destIds:     ObsIdSet,
    expandedIds: View[SortedSet[ObsIdSet]],
    setObsSet:   ObsIdSet => Callback,
    allTargets:  TargetList
  )(using c: FetchClient[IO, ObservationDB]) =
    val traversal =
      Iso
        .id[ObservationList]
        .filterIndex((id: Observation.Id) => draggedIds.contains(id))
        .andThen(KeyedIndexedList.value)
        .andThen(ObsSummary.scienceTargetIds)

    Action(getter = traversal.getAll.andThen(_.head), setter = traversal.replace)(
      onSet = (observationList, asterismIds) =>
        // destination ids may not be found when undoing
        val filteredTargetIds = asterismIds.filter(allTargets.contains)
        AsterismQueries.replaceAsterism[IO](
          programId,
          draggedIds.toList,
          filteredTargetIds.toList
        ) // >> TODO THIS 2 THINGS. WE SHOULD FILTER OBS IDs TO STILL EXISTING ONES
        // expandedIds.mod(updateExpandedIds(draggedIds, optDestIds) _).to[IO] >>
        // setObsSet(optDestIds.fold(draggedIds)(_ ++ draggedIds)).to[IO]
      // }
    )
}
