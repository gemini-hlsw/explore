// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order._
import cats.effect.Async
import cats.syntax.all._
import clue.TransactionalClient
import explore.common.TargetListGroupQueries
import explore.common.TargetListGroupQueries._
import explore.implicits._
import explore.model.SelectedPanel
import explore.model.SelectedPanel.Editor
import explore.model.TargetEnv
import explore.model.TargetEnvIdObsIdSet
import explore.undo._
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB

import scala.collection.immutable.SortedSet

object TargetListGroupObsListActions {
  private def getter(
    draggedIds: TargetEnvIdObsIdSet
  ): TargetListGroupList => Option[TargetEnv] =
    _.values
      .find(_.id.intersect(draggedIds).nonEmpty)

  private def setter(draggedIds: TargetEnvIdObsIdSet, targetIds: Set[Target.Id])(
    oTargetEnv:                  Option[TargetEnv]
  ): TargetListGroupList => TargetListGroupList = originalGroupList => {
    val originalEnvList = originalGroupList.values

    // should always have a targetEnvironment and be able to find the dragged Ids in the list
    (oTargetEnv, originalEnvList.find(_.id.intersect(draggedIds).nonEmpty))
      .mapN { case (destEnv, srcEnv) =>
        val tempList    = originalGroupList - srcEnv.id
        val updatedList = srcEnv.filterOutIds(draggedIds, targetIds).fold(tempList) { filteredEnv =>
          tempList + filteredEnv.asObsKeyValue
        }

        originalEnvList
          .find(_.areScienceTargetsEqual(destEnv))
          .fold(updatedList + destEnv.asObsKeyValue) { newEnv =>
            // I can only add the TargetEnvIdObsIds from src to dest since I won't know what the new
            // target ids will be. Will need a server trip to be fully updated unless there are no targets.
            updatedList - newEnv.id + newEnv.addIds(draggedIds).asObsKeyValue
          }
      }
      .getOrElse(originalGroupList)
  }

  private def updateExpandedIds(
    draggedIds: TargetEnvIdObsIdSet,
    optDestIds: Option[TargetEnvIdObsIdSet]
  )(
    eids:       SortedSet[TargetEnvIdObsIdSet]
  ) =
    optDestIds.fold(
      eids.flatMap(ids => ids.remove(draggedIds)) + draggedIds
    ) { destIds =>
      eids.flatMap(ids =>
        if (ids === destIds) none
        else ids.remove(draggedIds)
      ) + (destIds ++ draggedIds)
    }

  private def updateSelected(
    draggedIds: TargetEnvIdObsIdSet,
    optDestIds: Option[TargetEnvIdObsIdSet]
  )(
    selected:   SelectedPanel[TargetEnvIdObsIdSet]
  ) =
    selected match {
      // If in edit mode, always edit the destination.
      case Editor(_) =>
        Editor(optDestIds.fold(draggedIds)(_ ++ draggedIds))
      case _         => selected
    }

  def obsTargetListGroup[F[_]](
    draggedIds:     TargetEnvIdObsIdSet,
    targetIds:      Set[Target.Id], // target ids for the dragged ids.
    expandedIds:    View[SortedSet[TargetEnvIdObsIdSet]],
    selected:       View[SelectedPanel[TargetEnvIdObsIdSet]]
  )(implicit async: Async[F], c: TransactionalClient[F, ObservationDB]) =
    Action[F](getter = getter(draggedIds), setter = setter(draggedIds, targetIds))(
      onSet = (tlgl, oTargetEnv) =>
        oTargetEnv.fold(async.unit) { tenv =>
          // destination ids may not be found when undoing
          val optDestIds = tlgl.values.find(_.areScienceTargetsEqual(tenv)).map(_.id)
          TargetListGroupQueries.replaceScienceTargetList[F](draggedIds.toList.map(_.targetEnvId),
                                                             tenv.scienceTargets.values.toList
          ) >>
            expandedIds.mod(updateExpandedIds(draggedIds, optDestIds) _).to[F] >>
            selected.mod(updateSelected(draggedIds, optDestIds) _).to[F]
        }
    )
}
