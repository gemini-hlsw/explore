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
import explore.model.AsterismGroup
import explore.model.ObsIdSet
import explore.model.TargetWithObs
import explore.model.syntax.all.*
import explore.undo.*
import japgolly.scalajs.react.callback.Callback
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import queries.common.TargetQueriesGQL

import scala.annotation.unused
import scala.collection.immutable.SortedSet

object AsterismGroupObsListActions {
  private def obsDropGetter(
    draggedIds: ObsIdSet
  ): AsterismGroupsWithObs => Option[AsterismGroup] =
    _.asterismGroups.findContainingObsIds(draggedIds)

  private def obsDropSetter(draggedIds: ObsIdSet, srcIds: ObsIdSet, destIds: ObsIdSet)(
    oAsterismGroup: Option[AsterismGroup]
  ): AsterismGroupsWithObs => AsterismGroupsWithObs = agwo => {
    val origAsterismGroups = agwo.asterismGroups

    // should always have an asterism group and be able to find the dragged Ids in the list
    (oAsterismGroup, origAsterismGroups.findContainingObsIds(draggedIds))
      .mapN { case (destGroup, srcGroup) =>
        // no matter what, the src group goes away because all or some of the obs were moved.
        val tempList    = origAsterismGroups - srcGroup.obsIds
        // if we didn't move all of the obs, we need to add the remnant back into the list
        val updatedList =
          srcGroup.removeObsIds(draggedIds).fold(tempList) { filteredGroup =>
            tempList + filteredGroup.asObsKeyValue
          }

        val updatedGroupList = if (srcGroup.obsIds === srcIds) {
          // do/redo - we have to remove the dest and add it back with the extra observations
          // what is really in the destination is a group with the destIds
          updatedList - destIds + AsterismGroup(destIds ++ draggedIds,
                                                destGroup.targetIds
          ).asObsKeyValue
        } else if (destGroup.obsIds === srcIds) {
          // undo - we have to put the destination group back and, if it exists, remove the
          // group with (srcIds - draggedIds)
          val tmp = updatedList + destGroup.asObsKeyValue
          srcIds.remove(draggedIds).fold(tmp)(ids => tmp - ids)
        } else updatedList // something changed outside the scope of this undoctx

        // remove the dragged ids from all the targets in the src
        val tempTargets = srcGroup.targetIds.foldRight(agwo.targetsWithObs) {
          case (tid, targetWithObs) =>
            targetWithObs.updatedWith(tid)(_.map(_.removeObsIds(draggedIds)))
        }

        // add the dragged ids to all the targets in the destination
        val updatedTargetsWithObs = destGroup.targetIds.foldRight(tempTargets) {
          case (tid, targetWithObs) =>
            targetWithObs.updatedWith(tid)(_.map(_.addObsIds(draggedIds)))
        }

        agwo.copy(asterismGroups = updatedGroupList, targetsWithObs = updatedTargetsWithObs)
      }
      .getOrElse(agwo)
  }

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
    setObsSet:   ObsIdSet => Callback
  )(using c: FetchClient[IO, ?, ObservationDB]) =
    Action(getter = obsDropGetter(draggedIds), setter = obsDropSetter(draggedIds, srcIds, destIds))(
      onSet = (agwo, oAsterismGroup) =>
        oAsterismGroup.foldMap { asterismGroup =>
          // destination ids may not be found when undoing
          val optDestIds =
            agwo.asterismGroups.findWithTargetIds(asterismGroup.targetIds).map(_.obsIds)
          AsterismQueries.replaceAsterism[IO](
            programId,
            draggedIds.toList,
            asterismGroup.targetIds.toList
          ) >>
            expandedIds.mod(updateExpandedIds(draggedIds, optDestIds) _).to[IO] >>
            setObsSet(optDestIds.fold(draggedIds)(_ ++ draggedIds)).to[IO]
        }
    )
}
