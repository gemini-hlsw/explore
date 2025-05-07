// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import crystal.react.*
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ObservationList
import explore.model.TargetList
import explore.services.OdbAsterismApi
import explore.undo.*
import japgolly.scalajs.react.callback.Callback
import lucuma.core.model.Target
import monocle.Iso
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedSet

object AsterismGroupObsListActions {
  def dropObservations(
    draggedIds:  ObsIdSet,
    srcIds:      ObsIdSet,
    destIds:     ObsIdSet,
    expandedIds: View[SortedSet[ObsIdSet]],
    setObsSet:   ObsIdSet => Callback,
    allTargets:  TargetList
  )(using odbApi: OdbAsterismApi[IO])(using Logger[IO]) =
    val traversal =
      Iso
        .id[ObservationList]
        .filterIndex((id: Observation.Id) => draggedIds.contains(id))
        .andThen(Observation.scienceTargetIds)

    Action(getter = traversal.getAll.andThen(_.head), setter = traversal.replace)(
      onSet = (_, asterismIds) =>
        // destination ids may not be found when undoing
        val filteredTargetIds: SortedSet[Target.Id] = asterismIds.filter(allTargets.contains)
        val destGroup: ObsIdSet                     = destIds ++ draggedIds

        expandedIds.async.mod(ids =>
          val base = ids - draggedIds - destIds + destGroup
          (srcIds -- draggedIds).fold(base)(base + _)
        ) >>
          odbApi.replaceAsterism(draggedIds.toList, filteredTargetIds.toList) >>
          setObsSet(destGroup).toAsync
    )
}
