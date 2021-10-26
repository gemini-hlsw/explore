// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.data.NonEmptySet
import cats.effect.Async
import cats.implicits._
import clue.TransactionalClient
import explore.common.ConstraintGroupQueries._
import explore.common.ObsQueries
import explore.implicits._
import explore.model.ConstraintGroup
import explore.model.ConstraintSet
import explore.model.ObsIdSet
import explore.model.SelectedPanel
import explore.model.SelectedPanel.Editor
import explore.undo._
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB

import scala.collection.immutable.SortedSet

object ConstraintGroupObsListActions {
  private def getter(obsId: Observation.Id): ConstraintGroupList => Option[ConstraintSet] =
    cgl => cgl.values.find(_.obsIds.contains(obsId)).map(_.constraintSet)

  private def setter(
    obsId: Observation.Id
  )(ocs:   Option[ConstraintSet]): ConstraintGroupList => ConstraintGroupList = cgl => {
    val constraintGroups = cgl.values

    val updatedCgl = constraintGroups.find(_.obsIds.contains(obsId)).fold(cgl) { oldCg =>
      val updatedOldCg = oldCg.removeObsId(obsId)
      val newList      = cgl - oldCg.obsIds
      if (updatedOldCg.obsIds.isEmpty) newList else newList + updatedOldCg.asKeyValue
    }

    ocs.fold(updatedCgl) { cs =>
      constraintGroups
        .find(_.constraintSet === cs)
        .fold(updatedCgl + ConstraintGroup(cs, NonEmptySet.one(obsId)).asKeyValue) { newCg =>
          val updatedNewCg = newCg.addObsId(obsId)
          updatedCgl - newCg.obsIds + updatedNewCg.asKeyValue
        }
    }
  }

  private def updateExpandedIds(
    obsId:      Observation.Id,
    optDestIds: Option[ObsIdSet]
  )(
    eids:       SortedSet[ObsIdSet]
  ) = {
    val setOfOne = NonEmptySet.one(obsId)

    optDestIds.fold(
      eids.map(ids =>
        if (ids =!= setOfOne) NonEmptySet.fromSetUnsafe(ids - obsId)
        else ids
      ) + setOfOne
    ) { destIds =>
      eids.flatMap(ids =>
        if (ids === destIds || ids === setOfOne) none
        else NonEmptySet.fromSetUnsafe(ids - obsId).some
      ) + destIds.add(obsId)
    }
  }

  private def updateSelected(obsId: Observation.Id, optDestIds: Option[ObsIdSet])(
    selected:                       SelectedPanel[ObsIdSet]
  ) =
    selected match {
      // If in edit mode, always edit the destination.
      case Editor(_) => Editor(optDestIds.fold(NonEmptySet.one(obsId))(_.add(obsId)))
      case _         => selected
    }

  def obsConstraintGroup[F[_]](
    obsId:          Observation.Id,
    expandedIds:    View[SortedSet[ObsIdSet]],
    selected:       View[SelectedPanel[ObsIdSet]]
  )(implicit async: Async[F], c: TransactionalClient[F, ObservationDB]) =
    Action[F](getter = getter(obsId), setter = setter(obsId))(
      onSet = (cgl, ocs) =>
        ocs.fold(async.unit) { cs =>
          // destination ids may not be found when undoing
          val optDestIds = cgl.values
            .find(_.constraintSet === cs)
            .map(_.obsIds)
          ObsQueries.updateObservationConstraintSet[F](obsId, cs) >>
            expandedIds.mod(updateExpandedIds(obsId, optDestIds) _).to[F] >>
            selected.mod(updateSelected(obsId, optDestIds) _).to[F]
        }
    )
}
