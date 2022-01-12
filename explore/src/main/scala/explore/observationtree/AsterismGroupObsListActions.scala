// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order._
import cats.effect.IO
import cats.syntax.all._
import clue.TransactionalClient
import crystal.react.View
import crystal.react.implicits._
import explore.common.AsterismQueries
import explore.common.AsterismQueries._
import explore.implicits._
import explore.model.AsterismGroup
import explore.model.FocusedObs
import explore.model.ObsIdSet
import explore.model.SelectedPanel
import explore.model.SelectedPanel.Editor
import explore.undo._
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import mouse.boolean._

import scala.annotation.unused
import scala.collection.immutable.SortedSet

object AsterismGroupObsListActions {
  private def obsDropGetter(
    draggedIds: ObsIdSet
  ): AsterismGroupList => Option[AsterismGroup] =
    _.findContainingObsIds(draggedIds)

  private def obsDropSetter(draggedIds: ObsIdSet)(
    oAsterismGroup:                     Option[AsterismGroup]
  ): AsterismGroupList => AsterismGroupList = originalGroupList =>
    // should always have an asterism group and be able to find the dragged Ids in the list
    (oAsterismGroup, originalGroupList.findContainingObsIds(draggedIds))
      .mapN { case (destGroup, srcGroup) =>
        val tempList    = originalGroupList - srcGroup.obsIds
        val updatedList =
          srcGroup.removeObsIds(draggedIds).fold(tempList) { filteredGroup =>
            tempList + filteredGroup.asObsKeyValue
          }

        originalGroupList
          .findWithTargetIds(destGroup.targetIds)
          .fold(updatedList + destGroup.asObsKeyValue) { newGroup =>
            updatedList - newGroup.obsIds + newGroup.addObsIds(draggedIds).asObsKeyValue
          }
      }
      .getOrElse(originalGroupList)

  // Any value returned by using this is wrong under too many circumstances. So, we always just get
  // the most current value from the list.
  private val targetDropGetter: AsterismGroupList => Option[AsterismGroup] = _ => None

  private def targetDropSetter(obsIds: ObsIdSet, targetId: Target.Id)(
    // We always need to get the most recent group from the list, so we ignore this.
    @unused oAsterismGroup:            Option[AsterismGroup]
  ): AsterismGroupList => AsterismGroupList = originalList =>
    // Note: We always need to get the most current asterism group from the list.
    originalList
      .findContainingObsIds(obsIds)
      .fold(originalList) { currentAg =>
        val currentObsIds = currentAg.obsIds
        // It seems the only way to know if we're setting or restoring is if the asterism group
        // currently contains the target id or not.
        val newTargetIds  = currentAg.targetIds
          .contains(targetId)
          .fold(currentAg.targetIds - targetId, currentAg.targetIds + targetId)

        val split = if (currentObsIds === obsIds) {
          originalList + currentAg.copy(targetIds = newTargetIds).asObsKeyValue
        } else {
          originalList - currentObsIds +
            currentAg.removeObsIdsUnsafe(obsIds).asObsKeyValue +
            AsterismGroup(obsIds, newTargetIds).asObsKeyValue
        }

        originalList.findWithTargetIds(newTargetIds).fold(split) { ag =>
          split - obsIds - ag.obsIds + ag.addObsIds(obsIds).asObsKeyValue
        }
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

  private def doTargetDropExpandedMod(
    obsIds:    ObsIdSet,
    targetId:  Target.Id,
    currentAg: AsterismGroup,
    agl:       AsterismGroupList
  )(eids:      SortedSet[ObsIdSet]): SortedSet[ObsIdSet] = {
    val newTargets = currentAg.targetIds + targetId
    agl.findWithTargetIds(newTargets).fold(eids) { mergeWith =>
      if (eids.contains(obsIds) || eids.contains(mergeWith.obsIds))
        eids - obsIds - mergeWith.obsIds + (obsIds ++ mergeWith.obsIds)
      else eids
    }
  }

  private def undoTargetDropExpandedMod(
    obsIds:    ObsIdSet,
    targetId:  Target.Id,
    currentAg: AsterismGroup,
    agl:       AsterismGroupList
  )(eids:      SortedSet[ObsIdSet]): SortedSet[ObsIdSet] = {
    val split =
      if (obsIds.strictSubsetOf(currentAg.obsIds) && eids.contains(currentAg.obsIds))
        eids - currentAg.obsIds + obsIds + currentAg.obsIds.removeUnsafe(obsIds)
      else eids

    val newTargets = currentAg.targetIds - targetId
    agl.findWithTargetIds(newTargets).fold(split) { mergeWith =>
      if (eids.contains(obsIds) || eids.contains(mergeWith.obsIds))
        split - obsIds - mergeWith.obsIds + (obsIds ++ mergeWith.obsIds)
      else split
    }
  }

  private def updateSelected(
    selected:   View[SelectedPanel[ObsIdSet]],
    focusedObs: View[Option[FocusedObs]],
    draggedIds: ObsIdSet,
    optDestIds: Option[ObsIdSet]
  ) = {
    val ids     = optDestIds.fold(draggedIds)(_ ++ draggedIds)
    val focused = ids.single.map(FocusedObs(_))
    selected.mod(panel =>
      panel match {
        // If in edit mode, always edit the destination.
        case Editor(_) => Editor(ids)
        case _         => panel
      }
    ) >> focusedObs.set(focused)
  }

  def dropObservations(
    draggedIds:  ObsIdSet,
    expandedIds: View[SortedSet[ObsIdSet]],
    selected:    View[SelectedPanel[ObsIdSet]],
    focusedObs:  View[Option[FocusedObs]]
  )(implicit c:  TransactionalClient[IO, ObservationDB]) =
    Action(getter = obsDropGetter(draggedIds), setter = obsDropSetter(draggedIds))(
      onSet = (agl, oAsterismGroup) =>
        oAsterismGroup.foldMap { asterismGroup =>
          // destination ids may not be found when undoing
          val optDestIds = agl.findWithTargetIds(asterismGroup.targetIds).map(_.obsIds)
          AsterismQueries.replaceAsterism[IO](draggedIds.toList, asterismGroup.targetIds.toList) >>
            expandedIds.mod(updateExpandedIds(draggedIds, optDestIds) _).to[IO] >>
            updateSelected(selected, focusedObs, draggedIds, optDestIds).to[IO]
        }
    )

  def dropTarget(obsIds: ObsIdSet, targetId: Target.Id, expandedIds: View[SortedSet[ObsIdSet]])(
    implicit c:          TransactionalClient[IO, ObservationDB]
  ) = Action(getter = targetDropGetter, setter = targetDropSetter(obsIds, targetId))(
    onSet = (agl, _) => {
      // We always need to look in the list to get the most current aterism group. We
      // should always find one, unless an observation has been deleted, in which case we do nothing...
      val oCurrentAg = agl.findContainingObsIds(obsIds)
      oCurrentAg.foldMap { currentAg =>
        // This seems to be the only way to tell if we're doing or undoing?
        (if (currentAg.targetIds.contains(targetId))
           // undo
           expandedIds
             .mod(undoTargetDropExpandedMod(obsIds, targetId, currentAg, agl) _)
             .to[IO] >>
             AsterismQueries.removeTargetFromAsterisms[IO](obsIds.toList, targetId)
         else
           // do or redo
           expandedIds
             .mod(doTargetDropExpandedMod(obsIds, targetId, currentAg, agl) _)
             .to[IO] >>
             AsterismQueries.addTargetToAsterisms[IO](obsIds.toList, targetId))
      }
    }
  )
}
