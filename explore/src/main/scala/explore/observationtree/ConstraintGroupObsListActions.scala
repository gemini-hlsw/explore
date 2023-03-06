// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.implicits.*
import clue.FetchClient
import crystal.react.View
import crystal.react.implicits.*
import explore.common.ConstraintGroupQueries.*
import explore.model.ConstraintGroup
import explore.model.ObsIdSet
import explore.undo.*
import japgolly.scalajs.react.callback.Callback
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import queries.schemas.odb.ObsQueries

import scala.collection.immutable.SortedSet

object ConstraintGroupObsListActions {
  private def getter(draggedIds: ObsIdSet): ConstraintGroupList => Option[ConstraintGroup] =
    _.values.find(cg => draggedIds.subsetOf(cg.obsIds))

  private def setter(
    draggedIds: ObsIdSet
  )(ocg:        Option[ConstraintGroup]): ConstraintGroupList => ConstraintGroupList = originalGroupList =>
    {
      val constraintGroups = originalGroupList.values

      // should always have a constraint group and be able to find the dragged ids in the list
      (ocg, constraintGroups.find(cg => draggedIds.subsetOf(cg.obsIds)))
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

  def obsConstraintGroup(
    programId:   Program.Id,
    draggedIds:  ObsIdSet,
    expandedIds: View[SortedSet[ObsIdSet]],
    setObsSet:   Option[ObsIdSet] => Callback
  )(implicit c:  FetchClient[IO, ?, ObservationDB]) =
    Action(getter = getter(draggedIds), setter = setter(draggedIds))(
      onSet = (cgl, ocg) =>
        ocg.fold(IO.unit) { cg =>
          // destination ids may not be found when undoing
          val optDestIds = cgl.values
            .find(_.constraintSet === cg.constraintSet)
            .map(_.obsIds)
          ObsQueries.updateObservationConstraintSet[IO](
            programId,
            draggedIds.toList,
            cg.constraintSet
          ) >>
            expandedIds.mod(updateExpandedIds(draggedIds, optDestIds) _).to[IO] >>
            setObsSet(optDestIds.fold(draggedIds)(_ ++ draggedIds).some).to[IO]
        }
    )
}
