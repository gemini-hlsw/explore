// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.implicits._
import clue.TransactionalClient
import crystal.react.View
import crystal.react.implicits._
import explore.common.ConstraintGroupQueries._
import explore.common.ObsQueries
import explore.implicits._
import explore.model.ConstraintGroup
import explore.model.ObsIdSet
import explore.model.SelectedPanel
import explore.model.SelectedPanel.Editor
import explore.undo._
import lucuma.schemas.ObservationDB

import scala.collection.immutable.SortedSet

object ConstraintGroupObsListActions {
  private def getter(draggedIds: ObsIdSet): ConstraintGroupList => Option[ConstraintGroup] =
    _.values.find(_.obsIds.intersects(draggedIds))

  private def setter(
    draggedIds: ObsIdSet
  )(ocg:        Option[ConstraintGroup]): ConstraintGroupList => ConstraintGroupList = originalGroupList =>
    {
      val constraintGroups = originalGroupList.values

      // should always have a constraint group and be able to find the dragged ids in the list
      (ocg, constraintGroups.find(_.obsIds.intersects(draggedIds)))
        .mapN { case (destCg, srcCg) =>
          val tempList    = originalGroupList - srcCg.obsIds
          val updatedList =
            srcCg
              .removeObsIds(draggedIds)
              .fold(tempList)(updatedCg => tempList + updatedCg.asKeyValue)

          constraintGroups
            .find(_.constraintSet == destCg.constraintSet) // see if we're merging
            .fold(updatedList + destCg.asKeyValue) { newCg =>
              updatedList - newCg.obsIds + newCg.addObsIds(draggedIds).asKeyValue
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

  private def updateSelected(draggedIds: ObsIdSet, optDestIds: Option[ObsIdSet])(
    selected:                            SelectedPanel[ObsIdSet]
  ) =
    selected match {
      // If in edit mode, always edit the destination.
      case Editor(_) => Editor(optDestIds.fold(draggedIds)(_ ++ draggedIds))
      case _         => selected
    }

  def obsConstraintGroup(
    draggedIds:  ObsIdSet,
    expandedIds: View[SortedSet[ObsIdSet]],
    selected:    View[SelectedPanel[ObsIdSet]]
  )(implicit c:  TransactionalClient[IO, ObservationDB]) =
    Action(getter = getter(draggedIds), setter = setter(draggedIds))(
      onSet = (cgl, ocg) =>
        ocg.fold(IO.unit) { cg =>
          // destination ids may not be found when undoing
          val optDestIds = cgl.values
            .find(_.constraintSet === cg.constraintSet)
            .map(_.obsIds)
          ObsQueries.updateObservationConstraintSet[IO](draggedIds.toList, cg.constraintSet) >>
            expandedIds.mod(updateExpandedIds(draggedIds, optDestIds) _).to[IO] >>
            selected.mod(updateSelected(draggedIds, optDestIds) _).to[IO]
        }
    )
}
