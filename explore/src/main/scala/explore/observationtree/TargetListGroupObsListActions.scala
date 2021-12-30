// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order._
import cats.effect.IO
import cats.syntax.all._
import clue.TransactionalClient
import crystal.react.View
import crystal.react.implicits._
import explore.common.TargetListGroupQueries
import explore.common.TargetListGroupQueries._
import explore.implicits._
import explore.model.AsterismGroup
import explore.model.FocusedObs
import explore.model.ObsIdSet
import explore.model.SelectedPanel
import explore.model.SelectedPanel.Editor
import explore.undo._
import lucuma.schemas.ObservationDB

import scala.collection.immutable.SortedSet

object TargetListGroupObsListActions {
  private def getter(
    draggedIds: ObsIdSet
  ): AsterismGroupList => Option[AsterismGroup] =
    _.values
      .find(_.obsIds.intersects(draggedIds))

  private def setter(draggedIds: ObsIdSet)(
    oAsterismGroup:              Option[AsterismGroup]
  ): AsterismGroupList => AsterismGroupList = originalGroupList => {
    val originalEnvList = originalGroupList.values

    // should always have an asterism group and be able to find the dragged Ids in the list
    (oAsterismGroup, originalEnvList.find(_.obsIds.intersects(draggedIds)))
      .mapN { case (destGroup, srcGroup) =>
        val tempList    = originalGroupList - srcGroup.obsIds
        val updatedList =
          srcGroup.removeObsIds(draggedIds).fold(tempList) { filteredGroup =>
            tempList + filteredGroup.asObsKeyValue
          }

        originalEnvList
          .find(_.asterism == destGroup.asterism)
          .fold(updatedList + destGroup.asObsKeyValue) { newGroup =>
            updatedList - newGroup.obsIds + newGroup.addObsIds(draggedIds).asObsKeyValue
          }
      }
      .getOrElse(originalGroupList)
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

  private def updateSelected(
    selected:   View[SelectedPanel[ObsIdSet]],
    focusedObs: View[Option[FocusedObs]],
    draggedIds: ObsIdSet,
    optDestIds: Option[ObsIdSet]
  ) = {
    val ids     = optDestIds.fold(draggedIds)(_ ++ draggedIds)
    val focused = ids.firstAndOnly.map(FocusedObs(_))
    selected.mod(panel =>
      panel match {
        // If in edit mode, always edit the destination.
        case Editor(_) => Editor(ids)
        case _         => panel
      }
    ) >> focusedObs.set(focused)
  }

  def obsTargetListGroup(
    draggedIds:  ObsIdSet,
    expandedIds: View[SortedSet[ObsIdSet]],
    selected:    View[SelectedPanel[ObsIdSet]],
    focusedObs:  View[Option[FocusedObs]]
  )(implicit c:  TransactionalClient[IO, ObservationDB]) =
    Action(getter = getter(draggedIds), setter = setter(draggedIds))(
      onSet = (agl, oAsterismGroup) =>
        oAsterismGroup.fold(IO.unit) { asterismGroup =>
          // destination ids may not be found when undoing
          val optDestIds = agl.values.find(_.asterism === asterismGroup.asterism).map(_.obsIds)
          TargetListGroupQueries.replaceScienceTargetList[IO](draggedIds.toList,
                                                              asterismGroup.asterism.toList
          ) >>
            expandedIds.mod(updateExpandedIds(draggedIds, optDestIds) _).to[IO] >>
            updateSelected(selected, focusedObs, draggedIds, optDestIds).to[IO]
        }
    )
}
