// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order._
import cats.effect.IO
import cats.syntax.all._
import clue.data.syntax._
import clue.TransactionalClient
import crystal.react.View
import crystal.react.implicits._
import explore.common.AsterismQueries
import explore.common.AsterismQueries._
import explore.implicits._
import explore.model.AsterismGroup
import explore.model.ObsIdSet
import explore.model.TargetGroup
import explore.undo._
import japgolly.scalajs.react.callback.Callback
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import queries.common.TargetQueriesGQL

import scala.annotation.unused
import scala.collection.immutable.SortedSet

object AsterismGroupObsListActions {
  private def obsDropGetter(
    draggedIds: ObsIdSet
  ): AsterismGroupsWithObs => Option[AsterismGroup] =
    _.asterismGroups.findContainingObsIds(draggedIds)

  private def obsDropSetter(draggedIds: ObsIdSet)(
    oAsterismGroup:                     Option[AsterismGroup]
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

        // Look for an asterism group with the exact target ids. If we find it, it is a do/redo,
        // if we don't find it, it's an undo
        val updatedGroupList = origAsterismGroups
          .findWithTargetIds(destGroup.targetIds)
          .fold(
            // undo - we just have to put the destination group back
            updatedList + destGroup.asObsKeyValue
          ) { newGroup =>
            // do/redo - we have to remove the original and add it back with the extra observations
            updatedList - newGroup.obsIds + newGroup.addObsIds(draggedIds).asObsKeyValue
          }

        // remove the dragged ids from all the targets in the src
        val tempTargetGroups = srcGroup.targetIds.foldRight(agwo.targetGroups) {
          case (tid, groups) => groups.updatedWith(tid)(_.map(_.removeObsIds(draggedIds)))
        }

        // add the dragged ids to all the targets in the destination
        val updatedTargetGroups = destGroup.targetIds.foldRight(tempTargetGroups) {
          case (tid, groups) => groups.updatedWith(tid)(_.map(_.addObsIds(draggedIds)))
        }

        agwo.copy(asterismGroups = updatedGroupList, targetGroups = updatedTargetGroups)
      }
      .getOrElse(agwo)
  }

  // Any value returned by using this is wrong under too many circumstances. So, we always just get
  // the most current value from the list.
  private val targetDropGetter: AsterismGroupsWithObs => Option[AsterismGroup] = _ => None

  private def targetDropSetter(obsIds: ObsIdSet, targetId: Target.Id)(
    // We always need to get the most recent group from the list, so we ignore this.
    @unused oAsterismGroup:            Option[AsterismGroup]
  ): AsterismGroupsWithObs => AsterismGroupsWithObs = agwo => {
    val origAsterismGroups = agwo.asterismGroups

    // Note: We always need to get the most current asterism group from the list.
    origAsterismGroups
      .findContainingObsIds(obsIds)
      .fold(agwo) { currentAg =>
        val currentObsIds = currentAg.obsIds

        // It seems the only way to know if we're setting or restoring is if the asterism group
        // currently contains the target id or not.
        val isUndo       = currentAg.targetIds.contains(targetId)
        val newTargetIds =
          if (isUndo) currentAg.targetIds - targetId
          else currentAg.targetIds + targetId

        val splitAsterismGroups = if (currentObsIds === obsIds) {
          origAsterismGroups + currentAg.copy(targetIds = newTargetIds).asObsKeyValue
        } else {
          origAsterismGroups - currentObsIds +
            currentAg.removeObsIdsUnsafe(obsIds).asObsKeyValue +
            AsterismGroup(obsIds, newTargetIds).asObsKeyValue
        }

        val updatedAsterismGroups =
          origAsterismGroups.findWithTargetIds(newTargetIds).fold(splitAsterismGroups) { ag =>
            splitAsterismGroups - obsIds - ag.obsIds + ag.addObsIds(obsIds).asObsKeyValue
          }

        val updatedTargetGroups = agwo.targetGroups.updatedWith(targetId)(
          _.map(tg => if (isUndo) tg.removeObsIds(obsIds) else tg.addObsIds(obsIds))
        )

        agwo.copy(asterismGroups = updatedAsterismGroups, targetGroups = updatedTargetGroups)
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

  private def deleteTarget(targetId: Target.Id)(implicit
    c:                               TransactionalClient[IO, ObservationDB]
  ): IO[Unit] =
    TargetQueriesGQL.DeleteTargetMutation
      .execute[IO](
        DeleteTargetInput(select = TargetSelectInput(targetIds = List(targetId).assign))
      )
      .void

  private def undeleteTarget(targetId: Target.Id)(implicit
    c:                                 TransactionalClient[IO, ObservationDB]
  ): IO[Unit] =
    TargetQueriesGQL.UndeleteTargetMutation
      .execute[IO](
        UndeleteTargetInput(select = TargetSelectInput(targetIds = List(targetId).assign))
      )
      .void

  def dropObservations(
    draggedIds:  ObsIdSet,
    expandedIds: View[SortedSet[ObsIdSet]],
    setObsSet:   ObsIdSet => Callback
  )(implicit c:  TransactionalClient[IO, ObservationDB]) =
    Action(getter = obsDropGetter(draggedIds), setter = obsDropSetter(draggedIds))(
      onSet = (agwo, oAsterismGroup) =>
        oAsterismGroup.foldMap { asterismGroup =>
          // destination ids may not be found when undoing
          val optDestIds =
            agwo.asterismGroups.findWithTargetIds(asterismGroup.targetIds).map(_.obsIds)
          AsterismQueries.replaceAsterism[IO](draggedIds.toList, asterismGroup.targetIds.toList) >>
            expandedIds.mod(updateExpandedIds(draggedIds, optDestIds) _).to[IO] >>
            setObsSet(optDestIds.fold(draggedIds)(_ ++ draggedIds)).to[IO]
        }
    )

  def dropTarget(obsIds: ObsIdSet, targetId: Target.Id, expandedIds: View[SortedSet[ObsIdSet]])(
    implicit c:          TransactionalClient[IO, ObservationDB]
  ) = Action(getter = targetDropGetter, setter = targetDropSetter(obsIds, targetId))(
    onSet = (agwo, _) => {
      val agl        = agwo.asterismGroups
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

  def targetExistence(
    targetId: Target.Id
  )(implicit
    appCtx:   AppContextIO
  ) = // can switch appCtx to TransactionalClient when logging not required
    Action[AsterismGroupsWithObs, Option[TargetGroup]](
      getter = (agwo: AsterismGroupsWithObs) => agwo.targetGroups.get(targetId),
      setter = (otg: Option[TargetGroup]) =>
        (agwo: AsterismGroupsWithObs) =>
          if (agwo.targetGroups.contains(targetId)) {
            // Delete - don't need to worry about the asterism groups for delete
            val newGroups = agwo.targetGroups.removed(targetId)
            agwo.copy(targetGroups = newGroups)
          } else
            // Undelete: Should always have a TargetGroup at this point.
            otg.fold(agwo) { tg =>
              val newTargetGroups   = agwo.targetGroups.updated(targetId, tg)
              val obsInTg           = tg.obsIds
              // update the asterism groups
              val newAsterismGroups = agwo.asterismGroups
                .map { case (_, ag) =>
                  if (ag.obsIds.toSortedSet.intersect(obsInTg).nonEmpty) ag.addTargetId(targetId)
                  else ag
                }
                .toList
                .toSortedMap(_.obsIds)
              agwo.copy(targetGroups = newTargetGroups, asterismGroups = newAsterismGroups)
            }
    )(
      onSet = (_, otg) =>
        otg.fold(
          appCtx.logger.error("Creating targets in AsterismGroupObsListActions not yet supported")
        )(_ => deleteTarget(targetId).void),
      onRestore = (_, otg) =>
        otg.fold(deleteTarget(targetId).void) { _ =>
          undeleteTarget(targetId).void
        }
    )
}
